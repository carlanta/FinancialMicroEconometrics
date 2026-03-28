# =============================================================================
# Capitulo 5: Modelos Multifactoriales
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net
#   - SP500_bulk_close.csv (306 acciones)
# =============================================================================

library(tidyverse)
library(readxl)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 5: Multifactoriales\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

sp_bulk <- read_csv("data/SP500_bulk_close.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date))

idx_sp <- "^GSPC"
acciones_sp <- setdiff(names(sp_bulk), c("date", idx_sp))
cat("Acciones S&P 500:", length(acciones_sp), "\n")

# =============================================================================
# 2. FACTOR MOMENTUM
# =============================================================================

sp_monthly <- sp_bulk |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(across(-date, ~ {
    vals <- na.omit(.)
    if (length(vals) < 2) return(NA_real_)
    last(vals) / first(vals) - 1
  }), .groups = "drop")

meses <- sort(unique(sp_monthly$month))

momentum_returns <- map_dfr(13:length(meses), function(i) {
  past_months <- meses[(i-12):(i-1)]
  current_month <- meses[i]
  past_data <- sp_monthly |> filter(month %in% past_months)
  cum_ret <- past_data |>
    summarise(across(all_of(acciones_sp), ~ prod(1 + ., na.rm = TRUE) - 1))
  cum_vals <- unlist(cum_ret[1, ])
  cum_vals <- cum_vals[!is.na(cum_vals) & is.finite(cum_vals)]
  if (length(cum_vals) < 20) return(NULL)
  q <- quantile(cum_vals, probs = c(0.2, 0.8), na.rm = TRUE)
  winners <- names(cum_vals[cum_vals >= q[2]])
  losers  <- names(cum_vals[cum_vals <= q[1]])
  current <- sp_monthly |> filter(month == current_month)
  ret_win <- mean(unlist(current[1, winners]), na.rm = TRUE)
  ret_los <- mean(unlist(current[1, losers]), na.rm = TRUE)
  tibble(date = current_month, WML = ret_win - ret_los,
         winners = ret_win, losers = ret_los)
})

cat("\nFactor momentum:\n")
cat("  Meses:", nrow(momentum_returns), "\n")
cat("  WML medio mensual:", round(mean(momentum_returns$WML) * 100, 2), "%\n")
cat("  WML anualizado:", round(mean(momentum_returns$WML) * 12 * 100, 1), "%\n")

# =============================================================================
# 3. GRAFICO ACUMULADO
# =============================================================================

p1 <- momentum_returns |>
  mutate(cum_wml = cumprod(1 + WML) - 1) |>
  ggplot(aes(x = date, y = cum_wml)) +
  geom_line(color = "#2c3e50", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Rendimiento acumulado",
       title = "Factor Momentum (Q5 - Q1) - S&P 500") +
  theme_minimal(base_size = 13)

ggsave("output/fig_05_momentum.png", p1, width = 8, height = 5, dpi = 300)
cat("\n[OK] output/fig_05_momentum.png\n")

write_csv(momentum_returns, "data/ds_momentum_ch05.csv")
cat("[OK] Capitulo 5 completado.\n")
