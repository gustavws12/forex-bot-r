# Forex bot · EUR/USD · 15 min · R + MetaTrader 4

Automatización experimental: R calcula indicadores y genera una señal (`senal.csv`) cada 15 minutos
mediante el programador de tareas de Windows; un Expert Advisor en MQL4 lee el archivo y ejecuta la
orden en MT4. Incluye backtest y exploración de variables.

```
main.R                 Loop principal
estrategia.R           Indicadores y señal
investigar_features.R  Exploración de variables
backtest.R             Backtest y curva de capital
scheduler.R            Registra la tarea programada (una sola vez)
logger.R               Registro de eventos
```

Proyecto de prueba en cuenta demo; no constituye recomendación de inversión.

Autor: Gustavo A Romero
