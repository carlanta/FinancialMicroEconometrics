# =============================================================================
# Capitulo 12: Backtesting, Stress Testing y Validacion
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# =============================================================================

library(tidyverse)
library(readxl)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 12: Backtesting\n")
cat("========================================\n\n")

tickers <- c("SAN_MC", "AAPL", "TOYOTA", "EURUSD", "ORO")
ret_mat <- desk_returns_matrix(tickers)
w <- rep(1/5, 5)
ret_mat <- ret_mat |>
  mutate(r_port = as.matrix(pick(all_of(tickers))) %*% w)
r_port <- ret_mat$r_port

# Backtesting rolling
n <- nrow(ret_mat)
bt_start <- n - 1500 + 1
ventana_est <- 500

bt_results <- map_dfr(bt_start:n, function(i) {
  sub <- r_port[(i - ventana_est):(i - 1)]
  r_actual <- r_port[i]
  var_norm <- -(mean(sub) + qnorm(0.01) * sd(sub))
  var_hist <- -quantile(sub, 0.01)
  tibble(date = ret_mat$date[i], r = r_actual,
         VaR_norm = var_norm, VaR_hist = as.numeric(var_hist),
         exc_norm = r_actual < -var_norm,
         exc_hist = r_actual < -as.numeric(var_hist))
})

n_bt <- nrow(bt_results)
cat(sprintf("Periodo: %s a %s (%d dias)\n",
            as.character(min(bt_results$date)), as.character(max(bt_results$date)), n_bt))
cat(sprintf("Excepciones esperadas: %.0f\n", n_bt * 0.01))
cat(sprintf("VaR Normal:    %d (%.2f%%)\n", sum(bt_results$exc_norm), sum(bt_results$exc_norm)/n_bt*100))
cat(sprintf("VaR Historico: %d (%.2f%%)\n", sum(bt_results$exc_hist), sum(bt_results$exc_hist)/n_bt*100))

# Grafico P&L vs VaR
p1 <- ggplot(bt_results, aes(x = date)) +
  geom_col(aes(y = r), fill = "grey70", width = 1) +
  geom_line(aes(y = -VaR_hist), color = "#c0392b", linewidth = 0.5) +
  geom_point(data = bt_results |> filter(exc_hist),
             aes(y = r), color = "#c0392b", size = 1.5) +
  labs(x = NULL, y = "Rendimiento diario",
       title = "Backtesting VaR 99% - Cartera multi-activo") +
  theme_minimal(base_size = 13)

ggsave("output/fig_12_backtest.png", p1, width = 8, height = 5, dpi = 300)
cat("\n[OK] output/fig_12_backtest.png\n")

# Semaforos Basilea (250 dias)
bt_250 <- bt_results |> tail(250)
exc_250 <- sum(bt_250$exc_hist)
zona <- ifelse(exc_250 <= 4, "VERDE", ifelse(exc_250 <= 9, "AMARILLA", "ROJA"))
cat(sprintf("\nSemaforo Basilea (250 dias): %d excepciones -> Zona %s\n", exc_250, zona))

# Stress tests
cat("\n--- Stress Tests ---\n")
crisis <- list(
  "Lehman 2008" = c("2008-09-01", "2008-10-31"),
  "COVID 2020"  = c("2020-03-01", "2020-03-31"),
  "Ucrania 2022" = c("2022-02-20", "2022-03-15"))

for (nm in names(crisis)) {
  rng <- as.Date(crisis[[nm]])
  sub <- ret_mat |> filter(date >= rng[1], date <= rng[2])
  if (nrow(sub) > 0) {
    cat(sprintf("  %-15s %2d dias  Perdida acum: %+.2f%%  Peor dia: %.2f%%\n",
                nm, nrow(sub), -sum(sub$r_port)*100, -min(sub$r_port)*100))
  }
}

cat("\n[OK] Capitulo 12 completado.\n")
cat("[OK] Manual completo.\n")
