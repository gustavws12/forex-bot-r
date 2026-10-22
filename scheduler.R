# ============================================================
#  scheduler.R
#  Programa main.R cada 15 minutos en Windows
#  Ejecutar este script UNA sola vez
# ============================================================

if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
  install.packages("taskscheduleR")
}

library(taskscheduleR)

# ── IMPORTANTE: ajusta esta ruta a donde guardaste el proyecto ──
RUTA_MAIN <- "C:/forex-bot/main.R"

taskscheduler_create(
  taskname  = "forex_bot_eurusd",
  rscript   = RUTA_MAIN,
  schedule  = "MINUTE",
  starttime = format(Sys.time() + 60, "%H:%M"),
  modifier  = 15,
  Rexe      = file.path(R.home("bin"), "Rscript.exe")
)

cat("✅ Scheduler activado: main.R se ejecutará cada 15 minutos\n")
cat("   Para ver tareas:   taskscheduler_ls()\n")
cat("   Para detener:      taskscheduler_delete('forex_bot_eurusd')\n")
