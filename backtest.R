# ============================================================
#  backtest.R
#  Validacion de la estrategia sobre datos REALES M15 de MT4
#  Simula si el precio toca primero el SL o el TP (realista)
#
#  Ejecutar:
#    setwd("C:/forex-bot")
#    source("backtest.R")
# ============================================================

library(quantmod)
library(TTR)
library(xts)
library(ggplot2)
library(dplyr)

# -- Leer precios reales M15 exportados por MT4 -----------------
MT4_FILES <- file.path(Sys.getenv("APPDATA"),
                       "MetaQuotes", "Terminal", "Common", "Files")
PRECIOS_FILE <- file.path(MT4_FILES, "precios.csv")

if (!file.exists(PRECIOS_FILE)) {
  stop("precios.csv no encontrado. Asegurate de que el EA este corriendo en MT4.")
}

df <- read.csv(PRECIOS_FILE, stringsAsFactors = FALSE)
df$time <- as.POSIXct(df$time, format = "%Y.%m.%d %H:%M", tz = "UTC")
df <- df[!is.na(df$time), ]
df <- df[order(df$time), ]

precio <- xts(df[, c("open","high","low","close","volume")], order.by = df$time)
colnames(precio) <- c("EURUSD.Open","EURUSD.High","EURUSD.Low","EURUSD.Close","EURUSD.Volume")

cat(sprintf("Velas M15 cargadas: %d  (%s  ->  %s)\n",
            nrow(precio),
            format(index(precio)[1]),
            format(index(precio)[nrow(precio)])))

# -- Indicadores ------------------------------------------------
cl <- Cl(precio); hi <- Hi(precio); lo <- Lo(precio); n_total <- length(cl)

sma_r   <- SMA(cl, 10)
sma_l   <- SMA(cl, 30)
rsi_v   <- RSI(cl, 14)
bb      <- BBands(cl, 20, sd = 2)
st      <- stoch(HLC(precio), 14, 3, 3)
stoch_k <- st[, "fastK"] * 100
stoch_d <- st[, "fastD"] * 100
atr_v   <- ATR(HLC(precio), 14)[, "atr"]

# -- Simulacion realista (SL/TP, una posicion a la vez) --------
ops <- data.frame()
i <- 35
pos_abierta_hasta <- 0

while (i <= n_total) {

  if (i <= pos_abierta_hasta) { i <- i + 1; next }  # esperar cierre

  px  <- as.numeric(cl[i]); r  <- as.numeric(rsi_v[i])
  sk  <- as.numeric(stoch_k[i]); sd_ <- as.numeric(stoch_d[i])
  sr  <- as.numeric(sma_r[i]); sl_ <- as.numeric(sma_l[i])
  bup <- as.numeric(bb[i,"up"]); bdn <- as.numeric(bb[i,"dn"])
  atr <- as.numeric(atr_v[i])

  if (any(is.na(c(px,r,sk,sd_,sr,sl_,bup,bdn,atr)))) { i <- i + 1; next }

  c_buy  <- sum(c(sr > sl_, r > 30 & r < 50, px <= bdn*1.001, sk < 30 & sk > sd_))
  c_sell <- sum(c(sr < sl_, r > 50 & r < 70, px >= bup*0.999, sk > 70 & sk < sd_))

  senal <- "HOLD"
  if (c_buy  >= 3) senal <- "BUY"
  if (c_sell >= 3) senal <- "SELL"

  if (senal == "HOLD") { i <- i + 1; next }

  # Definir SL / TP
  if (senal == "BUY") { sl_p <- px - atr*1.5; tp_p <- px + atr*3.0 }
  else                { sl_p <- px + atr*1.5; tp_p <- px - atr*3.0 }

  # Mirar hacia adelante: que toca primero, SL o TP
  resultado <- NA; pips <- NA; barras_dur <- NA
  for (j in (i+1):n_total) {
    hj <- as.numeric(hi[j]); lj <- as.numeric(lo[j])
    if (senal == "BUY") {
      if (lj <= sl_p) { resultado <- "perdida"; pips <- (sl_p-px)*10000; barras_dur <- j-i; break }
      if (hj >= tp_p) { resultado <- "ganada";  pips <- (tp_p-px)*10000; barras_dur <- j-i; break }
    } else {
      if (hj >= sl_p) { resultado <- "perdida"; pips <- (px-sl_p)*10000; barras_dur <- j-i; break }
      if (lj <= tp_p) { resultado <- "ganada";  pips <- (px-tp_p)*10000; barras_dur <- j-i; break }
    }
  }

  if (is.na(resultado)) { i <- i + 1; next }  # no cerro dentro del rango

  ops <- rbind(ops, data.frame(
    fecha = index(cl)[i], senal = senal, entrada = px,
    sl = sl_p, tp = tp_p, resultado = resultado,
    pips = pips, duracion_barras = barras_dur
  ))

  pos_abierta_hasta <- i + barras_dur   # bloquear hasta el cierre
  i <- i + 1
}

# -- Metricas ---------------------------------------------------
if (nrow(ops) == 0) {
  cat("\nNo se generaron operaciones en este periodo.\n")
  cat("Con solo ~3 dias de datos M15 es normal. Carga mas historial\n")
  cat("en MT4 (scroll a la izquierda en el grafico) y sube NumVelas en el EA.\n")
} else {
  total <- nrow(ops)
  gan   <- sum(ops$resultado == "ganada")
  wr    <- gan/total*100
  pnl   <- sum(ops$pips)
  ops$capital <- cumsum(ops$pips)
  max_dd <- min(c(0, cummax(ops$capital) - ops$capital)) * -1

  cat("\n============ RESULTADOS BACKTEST (M15 real) ============\n")
  cat(sprintf("Operaciones:    %d\n", total))
  cat(sprintf("Ganadas:        %d  | Perdidas: %d\n", gan, total-gan))
  cat(sprintf("Win rate:       %.1f%%\n", wr))
  cat(sprintf("PnL total:      %.1f pips\n", pnl))
  cat(sprintf("PnL medio/op:   %.1f pips\n", mean(ops$pips)))
  cat(sprintf("Max drawdown:   %.1f pips\n", max_dd))
  cat("========================================================\n")

  p <- ggplot(ops, aes(fecha, capital)) +
    geom_area(fill = "#00d4aa", alpha = 0.1) +
    geom_line(color = "#00d4aa", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#64748b") +
    geom_point(aes(color = resultado), size = 2.5) +
    scale_color_manual(values = c(ganada="#00d4aa", perdida="#f43f5e")) +
    labs(title = "Curva de Capital - Backtest EUR/USD M15",
         subtitle = sprintf("Win rate: %.1f%% | PnL: %.0f pips | Ops: %d", wr, pnl, total),
         x = NULL, y = "Pips acumulados", color = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.background = element_rect(fill="#0a0e17", color=NA),
          panel.background = element_rect(fill="#111827", color=NA),
          text = element_text(color="#e2e8f0"),
          axis.text = element_text(color="#64748b"),
          panel.grid = element_line(color="#1e2d40"),
          legend.background = element_rect(fill="#111827", color=NA))

  ggsave("curva_capital.png", plot = p, width = 12, height = 6, dpi = 150)
  cat("Grafica guardada en C:/forex-bot/curva_capital.png\n")
}
