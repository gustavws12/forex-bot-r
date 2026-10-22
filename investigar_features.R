# ============================================================
#  investigar_features.R
#  Harness de investigacion: mide si un feature PREDICE el
#  retorno futuro, ANTES de cablearlo al bot.
#
#  Filosofia: ningun feature entra a la estrategia hasta que
#  demuestre poder predictivo en una prueba FUERA DE MUESTRA.
#
#  Metrica principal: Information Coefficient (IC) = correlacion
#  de Spearman entre el feature (en t) y el retorno futuro (t -> t+h).
#  En finanzas un IC out-of-sample estable de 0.03-0.05 ya es algo;
#  cercano a 0 = el feature no sirve.
# ============================================================

suppressMessages({
  library(xts)
  library(TTR)
})

# ---- CONFIGURACION ----------------------------------------
# Carpeta con CSVs historicos LARGOS (ver nota de datos abajo).
# Por defecto usa la carpeta comun de MT4 (los precios_*.csv).
RESEARCH_DIR <- file.path(Sys.getenv("APPDATA"),
                          "MetaQuotes","Terminal","Common","Files")

TARGET   <- "EURUSD"   # simbolo a investigar (cambialo: "#BTCUSD", "XAGUSD"...)
HORIZON  <- 8          # barras a futuro para el retorno objetivo (8 x 15m = 2h)
TRAIN_FRAC <- 0.70     # 70% temporal para train, 30% final para test (OOS)

# ---- CARGA DE DATOS ---------------------------------------
cargar <- function(sym) {
  arch <- gsub("[^A-Za-z0-9_]", "", sym)
  pf <- file.path(RESEARCH_DIR, paste0("precios_", arch, ".csv"))
  if (!file.exists(pf)) return(NULL)
  d <- read.csv(pf, stringsAsFactors = FALSE)
  d$time <- as.POSIXct(d$time, format = "%Y.%m.%d %H:%M", tz = "UTC")
  d <- d[!is.na(d$time), ]
  d <- d[order(d$time), ]
  x <- xts(d[, c("open","high","low","close")], order.by = d$time)
  x[!duplicated(index(x)), ]
}

# Universo para features cross-asset
UNIVERSO <- c("EURUSD","USDJPY","EURJPY","GBPUSD","USDCHF","AUDUSD",
              "USDCAD","NZDUSD","XAUUSD","XAGUSD","USOIL","#BTCUSD",
              "USA30","USA100")

datos <- list()
for (s in UNIVERSO) { d <- cargar(s); if (!is.null(d)) datos[[s]] <- d }

if (is.null(datos[[TARGET]])) stop(paste("No hay datos de", TARGET))
cat(sprintf("Cargados %d simbolos. Objetivo: %s (%d barras)\n",
            length(datos), TARGET, nrow(datos[[TARGET]])))

if (nrow(datos[[TARGET]]) < 500)
  cat("\n*** ADVERTENCIA: pocas barras. Para conclusiones reales necesitas\n",
      "    miles de barras (ver nota de datos al final del script). ***\n\n")

# ---- COMPOSITES CROSS-ASSET -------------------------------
# Retorno log por simbolo, alineado en una sola matriz temporal
rets <- do.call(merge, lapply(datos, function(x) diff(log(Cl(x)))))
colnames(rets) <- names(datos)
rets <- na.omit(rets)

# Fuerza del dolar: +ret si USD es base (USDxxx), -ret si es quote (xxxUSD)
usd_comp <- xts(rep(0, nrow(rets)), order.by = index(rets))
npairs <- 0
for (s in colnames(rets)) {
  if (grepl("^USD", s))      { usd_comp <- usd_comp + rets[, s]; npairs <- npairs + 1 }
  else if (grepl("USD$", s) && !grepl("BTC", s)) { usd_comp <- usd_comp - rets[, s]; npairs <- npairs + 1 }
}
if (npairs > 0) usd_comp <- usd_comp / npairs

# Sentimiento risk-on: +cripto/indices/commodity-fx, -refugios (JPY, CHF fuerza)
risk_comp <- xts(rep(0, nrow(rets)), order.by = index(rets))
add_if <- function(rc, s, sign) if (s %in% colnames(rets)) rc + sign*rets[, s] else rc
risk_comp <- add_if(risk_comp, "#BTCUSD", +1)
risk_comp <- add_if(risk_comp, "USA100",  +1)
risk_comp <- add_if(risk_comp, "USA30",   +1)
risk_comp <- add_if(risk_comp, "AUDUSD",  +1)
risk_comp <- add_if(risk_comp, "NZDUSD",  +1)
risk_comp <- add_if(risk_comp, "USDJPY",  +1)  # USDJPY sube en risk-on
risk_comp <- add_if(risk_comp, "USDCHF",  +1)  # USDCHF sube en risk-on

# ---- CONSTRUCCION DE FEATURES (sobre el TARGET) -----------
x  <- datos[[TARGET]]
cl <- Cl(x); hi <- Hi(x); lo <- Lo(x)
ret1 <- diff(log(cl))

F <- list()

# (A) Estructura de retornos: momentum vs reversion a varios lags
F[["ret_lag1"]]  <- lag(ret1, 1)
F[["ret_lag4"]]  <- lag(rollapply(ret1, 4, sum, align="right", fill=NA), 1)
F[["ret_lag8"]]  <- lag(rollapply(ret1, 8, sum, align="right", fill=NA), 1)
F[["ret_lag24"]] <- lag(rollapply(ret1, 24, sum, align="right", fill=NA), 1)

# (B) Regimen de volatilidad
atr <- ATR(cbind(hi,lo,cl), n=14)[,"atr"]
F[["atr_pctl"]] <- rollapply(atr, 100, function(v) mean(tail(v,1) >= v, na.rm=TRUE),
                             align="right", fill=NA)
vol_corto <- rollapply(ret1, 8,  sd, align="right", fill=NA)
vol_largo <- rollapply(ret1, 48, sd, align="right", fill=NA)
F[["vol_ratio"]] <- vol_corto / vol_largo

# (C) Posicion en el rango reciente (feature de reversion a la media)
hh <- rollapply(hi, 48, max, align="right", fill=NA)
ll <- rollapply(lo, 48, min, align="right", fill=NA)
F[["range_pos"]] <- (cl - ll) / (hh - ll)   # 0=piso, 1=techo

# (D) Temporales / sesion
hora <- as.numeric(format(index(x), "%H"))
F[["hora_sin"]] <- xts(sin(2*pi*hora/24), order.by=index(x))
F[["hora_cos"]] <- xts(cos(2*pi*hora/24), order.by=index(x))
dow <- as.numeric(format(index(x), "%w"))
F[["dia_semana"]] <- xts(dow, order.by=index(x))

# (E) Cross-asset (alineado al target)
ali <- function(serie) {
  m <- merge(xts(rep(NA,nrow(x)), order.by=index(x)), serie)[,2]
  na.locf(m, na.rm=FALSE)
}
F[["usd_strength"]] <- ali(usd_comp)
F[["risk_onoff"]]   <- ali(risk_comp)
# beta movil del target al dolar (signo de su relacion reciente)
tr <- ali(diff(log(Cl(datos[[TARGET]]))))
F[["corr_usd"]] <- rollapply(merge(tr, ali(usd_comp)), 48,
                             function(w) { w<-na.omit(w); if(nrow(w)<10) NA else cor(w[,1],w[,2]) },
                             by.column=FALSE, align="right", fill=NA)

# ---- TARGET: retorno futuro a HORIZON barras --------------
fwd <- (lag(cl, -HORIZON) - cl) / cl   # retorno hacia adelante

# ---- MATRIZ Y LIMPIEZA ------------------------------------
M <- do.call(merge, c(F, list(fwd = fwd)))
colnames(M) <- c(names(F), "fwd")
M <- na.omit(M)
n <- nrow(M)
if (n < 50) stop("Muy pocos datos validos tras construir features.")

# ---- SPLIT TEMPORAL TRAIN / TEST (out-of-sample) ----------
corte <- floor(n * TRAIN_FRAC)
tr_idx <- 1:corte
te_idx <- (corte+1):n
se_ic  <- function(k) 1/sqrt(k)   # error estandar aprox del IC

# ---- IC POR FEATURE (Spearman) ----------------------------
ic <- function(f, y) {
  ok <- is.finite(f) & is.finite(y)
  if (sum(ok) < 20) return(NA)
  suppressWarnings(cor(f[ok], y[ok], method="spearman"))
}

res <- data.frame(feature=names(F), IC_train=NA, IC_test=NA, signif_test=NA)
for (i in seq_along(F)) {
  fcol <- as.numeric(M[, names(F)[i]])
  y    <- as.numeric(M[, "fwd"])
  res$IC_train[i] <- ic(fcol[tr_idx], y[tr_idx])
  res$IC_test[i]  <- ic(fcol[te_idx], y[te_idx])
}
se <- se_ic(length(te_idx))
res$signif_test <- ifelse(abs(res$IC_test) > 2*se, "SI", "no")

# Ordenar por |IC_test|
res <- res[order(-abs(res$IC_test)), ]

cat("\n=================================================================\n")
cat(sprintf(" PODER PREDICTIVO DE FEATURES  |  %s  |  horizonte %d barras\n", TARGET, HORIZON))
cat(sprintf(" Train: %d obs   Test (OOS): %d obs   SE_IC ~ %.3f\n", length(tr_idx), length(te_idx), se))
cat("=================================================================\n")
cat(sprintf("%-14s %9s %9s %10s\n", "feature", "IC_train", "IC_test", "signif?"))
cat(paste(rep("-",55), collapse=""), "\n")
for (i in 1:nrow(res)) {
  cat(sprintf("%-14s %+9.4f %+9.4f %10s\n",
              res$feature[i], res$IC_train[i], res$IC_test[i], res$signif_test[i]))
}
cat(paste(rep("-",55), collapse=""), "\n")
cat("Lectura: 'signif? = SI' significa |IC_test| > 2 errores estandar,\n")
cat("es decir, improbable que sea puro azar. Pero ojo con el multiple\n")
cat("testing: probando", length(F), "features, ~1 puede salir 'SI' por suerte.\n")
cat("Lo solido es que IC_train e IC_test tengan el MISMO signo y magnitud.\n")

# ---- ANALISIS POR QUINTILES DEL MEJOR FEATURE (en TEST) ---
best <- res$feature[1]
cat(sprintf("\n--- Analisis por quintiles (OOS) del mejor feature: %s ---\n", best))
fv <- as.numeric(M[te_idx, best]); yv <- as.numeric(M[te_idx, "fwd"])
ok <- is.finite(fv) & is.finite(yv); fv<-fv[ok]; yv<-yv[ok]
q <- cut(fv, breaks=quantile(fv, probs=seq(0,1,0.2), na.rm=TRUE),
         include.lowest=TRUE, labels=c("Q1(bajo)","Q2","Q3","Q4","Q5(alto)"))
cat(sprintf("%-10s %8s %10s\n", "quintil", "n", "fwd_ret_medio"))
for (lv in levels(q)) {
  sel <- q==lv
  cat(sprintf("%-10s %8d %+10.5f\n", lv, sum(sel), mean(yv[sel])))
}
cat("\nSi el feature predice, el retorno medio deberia subir (o bajar)\n")
cat("de forma MONOTONA de Q1 a Q5. Si es plano/erratico, no sirve.\n")

# ============================================================
#  NOTA DE DATOS (IMPORTANTE):
#  Los precios_*.csv solo traen ~300 barras (3 dias) = INSUFICIENTE.
#  Para investigar en serio necesitas MILES de barras. Opciones:
#   1) En el EA, sube NumVelas a 5000, recarga, y haz scroll (Inicio)
#      en cada grafico para que MT4 baje historial; reexporta.
#   2) O exporta desde MT4: F2 (Historial), elige simbolo+M15, exporta.
#  Copia esos CSVs largos a una carpeta y apunta RESEARCH_DIR ahi.
# ============================================================
