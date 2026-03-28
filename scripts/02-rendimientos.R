# =============================================================================
# Capitulo 2: Propiedades Estadisticas de los Rendimientos Financieros
# Econometria del Riesgo Financiero
# =============================================================================
#
# Autor: Carlos de Anta Puig
#        carlos@cwconsultores.com
#
# Datos: desk.carlosdeanta.net
#   - TEF_MC.xlsx, EURUSD.xlsx, ORO.xlsx, BTC.xlsx
#
# =============================================================================

library(tidyverse)
library(readxl)
library(moments)
library(tseries)
library(kableExtra)

source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 2: Rendimientos\n")
cat("  Hechos estilizados\n")
cat("========================================\n\n")

# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================

ret_tef <- desk_returns("TEF_MC", type = "log")
ret_eur <- desk_returns("EURUSD", type = "log")
ret_oro <- desk_returns("ORO", type = "log")
ret_btc <- desk_returns("BTC", type = "log")

activos <- list("TEF.MC" = ret_tef, "EUR/USD" = ret_eur,
                "Oro" = ret_oro, "Bitcoin" = ret_btc)

# =============================================================================
# 2. ACF DE RENDIMIENTOS Y CUADRADOS
# =============================================================================

png("output/fig_02_acf_rendimientos.png", width = 800, height = 600, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (nm in names(activos)) {
  acf(activos[[nm]]$return, main = nm, lag.max = 30, col = "#2c3e50")
}
dev.off()
cat("[OK] output/fig_02_acf_rendimientos.png\n")

png("output/fig_02_acf_cuadrados.png", width = 800, height = 600, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (nm in names(activos)) {
  acf(activos[[nm]]$return^2, main = paste(nm, "- r2"), lag.max = 30, col = "#c0392b")
}
dev.off()
cat("[OK] output/fig_02_acf_cuadrados.png\n")

# =============================================================================
# 3. QQ-PLOTS
# =============================================================================

png("output/fig_02_qqplots.png", width = 800, height = 600, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (nm in names(activos)) {
  qqnorm(activos[[nm]]$return, main = nm, col = "#2980b9", pch = 16, cex = 0.4)
  qqline(activos[[nm]]$return, col = "#c0392b", lwd = 2)
}
dev.off()
cat("[OK] output/fig_02_qqplots.png\n")

# =============================================================================
# 4. TESTS FORMALES
# =============================================================================

cat("\n--- Tests formales ---\n")
for (nm in names(activos)) {
  r <- activos[[nm]]$return
  jb <- jarque.bera.test(r)
  lb <- Box.test(r, lag = 10, type = "Ljung-Box")
  lb2 <- Box.test(r^2, lag = 10, type = "Ljung-Box")
  adf_t <- adf.test(r)
  cat(sprintf("%-10s JB p=%.4f  LB(10) p=%.4f  LB(10)r2 p=%.4f  ADF p=%.4f\n",
              nm, jb$p.value, lb$p.value, lb2$p.value, adf_t$p.value))
}

# =============================================================================
# 5. TABLA DESCRIPTIVA
# =============================================================================

desc <- map_dfr(names(activos), function(nm) {
  r <- activos[[nm]]$return
  tibble(Activo = nm, N = length(r), Media = mean(r), DE = sd(r),
         Min = min(r), Max = max(r),
         Asimetria = skewness(r), Curtosis = kurtosis(r))
})

cat("\n--- Estadisticos descriptivos ---\n")
print(desc |> mutate(across(where(is.numeric) & !matches("N"), ~round(., 4))))

write_csv(desc, "data/ds_descriptivos_ch02.csv")
cat("\n[OK] Capitulo 2 completado.\n")
