# =============================================================================
# Capitulo 10: Value at Risk
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - SAN_MC, AAPL, TOYOTA, EURUSD, ORO
# =============================================================================

library(tidyverse)
library(readxl)
library(rugarch)
library(MASS)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 10: Value at Risk\n")
cat("========================================\n\n")

tickers <- c("SAN_MC", "AAPL", "TOYOTA", "EURUSD", "ORO")
ret_mat <- desk_returns_matrix(tickers)
w <- rep(1/5, 5)
ret_mat <- ret_mat |>
  mutate(r_port = as.matrix(pick(all_of(tickers))) %*% w)
r_port <- ret_mat$r_port
cat("Cartera: 5 activos equiponderados,", nrow(ret_mat), "obs.\n")

# VaR parametrico
mu <- mean(r_port); sigma <- sd(r_port)
var_99_norm <- -(mu + qnorm(0.01) * sigma)
fit_t <- fitdistr(r_port, "t"); nu <- fit_t$estimate["df"]
var_99_t <- -(mu + qt(0.01, df = nu) * sigma * sqrt((nu - 2) / nu))

# VaR historico
var_99_hist <- -quantile(r_port, 0.01)

# VaR GARCH
spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
fit <- ugarchfit(spec, data = r_port, solver = "hybrid")
sigma_t <- as.numeric(tail(sigma(fit), 1))
nu_g <- coef(fit)["shape"]
var_99_garch <- -(0 + qt(0.01, df = nu_g) * sigma_t * sqrt((nu_g - 2) / nu_g))

cat("\n--- VaR 99% ---\n")
cat(sprintf("Normal:     %.4f (%.2f%%)\n", var_99_norm, var_99_norm * 100))
cat(sprintf("t-Student:  %.4f (%.2f%%)\n", var_99_t, var_99_t * 100))
cat(sprintf("Historico:  %.4f (%.2f%%)\n", as.numeric(var_99_hist), as.numeric(var_99_hist) * 100))
cat(sprintf("GARCH:      %.4f (%.2f%%)\n", var_99_garch, var_99_garch * 100))

# Diversificacion
var_indiv <- map_dbl(tickers, ~ -quantile(ret_mat[[.x]], 0.01))
var_suma <- sum(w * var_indiv)
cat(sprintf("\nDiversificacion: VaR cartera=%.4f vs suma=%.4f (beneficio %.1f%%)\n",
            as.numeric(var_99_hist), var_suma,
            (var_suma - as.numeric(var_99_hist)) / var_suma * 100))

cat("\n[OK] Capitulo 10 completado.\n")
