# ============================================================
#  FOREX BOT - main.R
#  Par: EUR/USD | Temporalidad: 15 min | Broker: MetaTrader 4
#  Estrategia: RSI + SMA + Bollinger + Stochastic + Soporte
#
#  AHORA lee precios REALES de M15 exportados por MT4
#  (no usa Yahoo). Lee y escribe en la carpeta comun de MT4.
# ============================================================

# Fijar el directorio del proyecto (necesario al correr via Task Scheduler,
# que no arranca en C:/forex-bot por defecto)
setwd("C:/forex-bot")

source("estrategia.R")
source("logger.R")

library(xts)

# -- Configuracion ----------------------------------------------
RIESGO_PCT <- 2      # % del balance a arriesgar por senal

# Carpeta COMUN de MT4 (ruta predecible, sin depender del usuario)
MT4_FILES <- file.path(Sys.getenv("APPDATA"),
                       "MetaQuotes", "Terminal", "Common", "Files")

PRECIOS_FILE <- file.path(MT4_FILES, "precios.csv")
SENAL_FILE   <- file.path(MT4_FILES, "senal.csv")

log_msg("============ INICIO CICLO ============")
log_msg(paste("Carpeta MT4:", MT4_FILES))

# -- 1. Leer precios reales exportados por MT4 ------------------
if (!file.exists(PRECIOS_FILE)) {
  log_msg("ERROR: precios.csv aun no existe.")
  log_msg("Verifica que el EA este corriendo en MT4 con AutoTrading activo.")
  stop("precios.csv no encontrado en la carpeta comun de MT4")
}

df <- read.csv(PRECIOS_FILE, stringsAsFactors = FALSE)

# Parsear fecha/hora (formato MT4: 2026.06.01 17:30)
df$time <- as.POSIXct(df$time, format = "%Y.%m.%d %H:%M", tz = "UTC")
df <- df[!is.na(df$time), ]
df <- df[order(df$time), ]

# Construir objeto xts OHLC con nombres que quantmod reconoce
precio <- xts(
  df[, c("open", "high", "low", "close", "volume")],
  order.by = df$time
)
colnames(precio) <- c("EURUSD.Open", "EURUSD.High",
                      "EURUSD.Low", "EURUSD.Close", "EURUSD.Volume")

log_msg(paste("Precios M15 leidos:", nrow(precio), "velas",
              "| ultima:", format(tail(index(precio), 1))))

# -- 2. Evaluar senal -------------------------------------------
resultado <- evaluar_senal(precio, riesgo_pct = RIESGO_PCT)

# -- 3. Escribir senal.csv en la carpeta comun de MT4 ----------
# Sin encabezado y sin comillas: el EA lee los campos directo de la
# primera linea (FileReadString en MT4 lee campo por campo).
write.table(resultado, SENAL_FILE, sep = ",",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

log_msg(paste(
  "Senal:", resultado$senal,
  "| Score:", paste0(resultado$score, "%"),
  "| Tecnico:", resultado$tech_dir,
  "| Modelos:", resultado$model_dir,
  "| Prob GBM:", resultado$prob
))

log_msg("============ FIN CICLO ===============")
