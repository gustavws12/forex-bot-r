# ============================================================
#  logger.R
# ============================================================

log_msg <- function(msg) {
  dir.create("logs", showWarnings = FALSE)
  linea <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
  cat(linea, "\n")
  write(linea, file = "logs/operaciones.log", append = TRUE)
}
