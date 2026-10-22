# ============================================================
#  estrategia.R  (indice combinado: indicadores + modelos)
#  Score 0-100 = 60% indicadores tecnicos + 40% modelos
#  El score define cuantas posiciones abrir (lo decide el EA)
# ============================================================

source("modelos.R")

library(quantmod)
library(TTR)
library(xts)

evaluar_senal <- function(precio, riesgo_pct = 2, umbral_min = 20) {

  precio <- na.approx(precio, na.rm = FALSE)
  precio <- na.omit(precio)

  cl <- Cl(precio)
  px <- as.numeric(tail(cl, 1))

  # --- Indicadores tecnicos: voto direccional en [-1, 1] ---
  sma_f <- as.numeric(tail(SMA(cl, 10), 1))
  sma_s <- as.numeric(tail(SMA(cl, 30), 1))
  rsi   <- as.numeric(tail(RSI(cl, 14), 1))
  bb    <- BBands(cl, 20, sd = 2)
  bb_up <- as.numeric(tail(bb[, "up"], 1))
  bb_mid<- as.numeric(tail(bb[, "mavg"], 1))
  st    <- stoch(HLC(precio), 14, 3, 3)
  k     <- as.numeric(tail(st[, "fastK"], 1)) * 100
  atr   <- as.numeric(tail(ATR(HLC(precio), 14)[, "atr"], 1))

  v_sma   <- max(-1, min(1, (sma_f - sma_s) / atr))     # tendencia
  v_rsi   <- (rsi - 50) / 50                            # momentum
  v_stoch <- (k - 50) / 50                              # momentum
  v_bb    <- max(-1, min(1, (px - bb_mid) / (bb_up - bb_mid)))  # posicion en banda
  tech_dir <- mean(c(v_sma, v_rsi, v_stoch, v_bb))

  # --- Modelos predictivos ---
  m <- calcular_probabilidad(precio)
  v_gbm   <- m$p_up - m$p_down
  v_kal   <- sign(m$kalman_pend)
  v_arima <- sign(m$arima_drift)
  model_dir <- mean(c(v_gbm, v_kal, v_arima))

  # --- Indice combinado: 60% indicadores / 40% modelos ---
  index_dir <- 0.6 * tech_dir + 0.4 * model_dir
  score <- abs(index_dir) * 100
  dir   <- sign(index_dir)

  r_dist <- atr * 1.5

  senal <- "HOLD"
  if (score >= umbral_min && dir != 0)
    senal <- ifelse(dir > 0, "BUY", "SELL")

  # CSV: el EA mapea score -> numero de posiciones y dimensiona al 2%
  data.frame(
    senal      = senal,
    score      = round(score, 1),
    r_dist     = round(r_dist, 5),
    riesgo_pct = riesgo_pct,
    dir_kalman = v_kal,
    dir_arima  = v_arima,
    tech_dir   = round(tech_dir, 3),
    model_dir  = round(model_dir, 3),
    prob       = round(max(m$p_up, m$p_down), 4),
    precio     = round(px, 5),
    timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}
