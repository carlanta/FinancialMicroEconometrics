# =============================================================================
# Capitulo 11: Expected Shortfall y Medidas Coherentes de Riesgo
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# =============================================================================

library(tidyverse)
library(readxl)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 11: Expected Shortfall\n")
cat("========================================\n\n")

tickers <- c("SAN_MC", "AAPL", "TOYOTA", "EURUSD", "ORO")
ret_mat <- desk_returns_matrix(tickers)
w <- rep(1/5, 5)
ret_mat <- ret_mat |>
  mutate(r_port = as.matrix(pick(all_of(tickers))) %*% w)
r_port <- ret_mat$r_port

# ES al 97.5% (FRTB) y 99%
var_975 <- -quantile(r_port, 0.025)
es_975 <- -mean(r_port[r_port <= -var_975])
var_99 <- -quantile(r_port, 0.01)
es_99 <- -mean(r_port[r_port <= -var_99])

cat(sprintf("VaR 97.5%%: %.4f  ES 97.5%%: %.4f  Ratio: %.2f\n", var_975, es_975, es_975/var_975))
cat(sprintf("VaR 99%%:   %.4f  ES 99%%:   %.4f  Ratio: %.2f\n", var_99, es_99, es_99/var_99))

# VaR vs ES rolling
ventana <- 500; n <- length(r_port)
var_es <- map_dfr(ventana:n, function(i) {
  sub <- r_port[(i - ventana + 1):i]
  tibble(date = ret_mat$date[i],
         VaR = -quantile(sub, 0.01),
         ES = -mean(sub[sub <= quantile(sub, 0.01)]))
})

p1 <- var_es |>
  pivot_longer(c(VaR, ES), names_to = "medida", values_to = "valor") |>
  ggplot(aes(x = date, y = valor * 100, color = medida)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = c("VaR" = "#2980b9", "ES" = "#c0392b")) +
  labs(x = NULL, y = "Perdida (%)", color = NULL, title = "VaR vs ES rolling (99%, 500 dias)") +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom")

ggsave("output/fig_11_var_es.png", p1, width = 8, height = 5, dpi = 300)
cat("\n[OK] output/fig_11_var_es.png\n")
cat("[OK] Capitulo 11 completado.\n")
