# =============================================================================
# Capitulo 5: Modelos Multifactoriales
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - SP500_bulk_close.csv
# =============================================================================

library(tidyverse)
library(readxl)
library(kableExtra)
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
  tibble(date = current_month,
         WML = mean(unlist(current[1, winners]), na.rm = TRUE) -
               mean(unlist(current[1, losers]), na.rm = TRUE))
})

cat("Factor momentum:", nrow(momentum_returns), "meses\n")
cat("WML medio:", round(mean(momentum_returns$WML) * 100, 2), "% mensual\n")

p1 <- momentum_returns |>
  mutate(cum_wml = cumprod(1 + WML) - 1) |>
  ggplot(aes(x = date, y = cum_wml)) +
  geom_line(color = "#2c3e50", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Rendimiento acumulado", title = "Momentum (Q5-Q1) S&P 500") +
  theme_minimal(base_size = 13)
ggsave("output/fig_05_momentum.png", p1, width = 8, height = 5, dpi = 300)
cat("[OK] output/fig_05_momentum.png\n")

# =============================================================================
# 3. REGRESIONES: CAPM vs MERCADO + MOMENTUM
# =============================================================================

cat("\n--- CAPM vs 2 factores ---\n")
sp_idx_monthly <- sp_monthly |> select(month, mkt = all_of(idx_sp)) |> drop_na()
acciones_validas <- acciones_sp[map_lgl(acciones_sp, ~ sum(!is.na(sp_monthly[[.x]])) > 80)]
set.seed(42)
acc_sel <- sample(acciones_validas, min(20, length(acciones_validas)))

df_reg <- sp_monthly |> select(month, all_of(acc_sel)) |>
  inner_join(sp_idx_monthly, by = "month") |>
  inner_join(momentum_returns |> select(date, WML) |> rename(month = date), by = "month")

comp <- map_dfr(acc_sel, function(tk) {
  df <- df_reg |> select(month, r = all_of(tk), mkt, WML) |> drop_na()
  if (nrow(df) < 24) return(NULL)
  m1 <- lm(r ~ mkt, data = df); m2 <- lm(r ~ mkt + WML, data = df)
  tibble(Ticker = tk, alpha_CAPM = round(coef(m1)[1], 4), R2_CAPM = round(summary(m1)$r.squared, 3),
         alpha_2F = round(coef(m2)[1], 4), beta_WML = round(coef(m2)["WML"], 3),
         R2_2F = round(summary(m2)$r.squared, 3))
})
print(comp)

# =============================================================================
# 4. FAMA-MACBETH
# =============================================================================

cat("\n--- Fama-MacBeth ---\n")
meses_ord <- sort(unique(df_reg$month))
n_half <- floor(length(meses_ord) / 2)
meses_1 <- meses_ord[1:min(36, n_half)]
meses_2 <- meses_ord[(length(meses_1)+1):length(meses_ord)]

betas_1 <- map_dfr(acc_sel, function(tk) {
  df <- df_reg |> filter(month %in% meses_1) |> select(r = all_of(tk), mkt, WML) |> drop_na()
  if (nrow(df) < 12) return(NULL)
  m <- lm(r ~ mkt + WML, data = df)
  tibble(Ticker = tk, beta_mkt = coef(m)["mkt"], beta_wml = coef(m)["WML"])
})

gammas <- map_dfr(meses_2, function(mes) {
  ret <- df_reg |> filter(month == mes) |> select(all_of(acc_sel)) |>
    pivot_longer(everything(), names_to = "Ticker", values_to = "r") |> drop_na()
  df_cs <- inner_join(ret, betas_1, by = "Ticker")
  if (nrow(df_cs) < 5) return(NULL)
  m <- lm(r ~ beta_mkt + beta_wml, data = df_cs)
  tibble(month = mes, gamma_0 = coef(m)[1], gamma_mkt = coef(m)["beta_mkt"],
         gamma_wml = coef(m)["beta_wml"])
})

cat("\nPrimas de riesgo:\n")
for (col in c("gamma_0", "gamma_mkt", "gamma_wml")) {
  v <- na.omit(gammas[[col]]); m <- mean(v); se <- sd(v)/sqrt(length(v))
  nm <- c(gamma_0="Intercepto", gamma_mkt="Mercado", gamma_wml="Momentum")[col]
  cat(sprintf("  %-12s Media=%.4f  EE=%.4f  t=%.3f\n", nm, m, se, m/se))
}

p2 <- ggplot(gammas, aes(x = month, y = gamma_wml)) +
  geom_col(fill = ifelse(gammas$gamma_wml > 0, "#2980b9", "#c0392b"), alpha = 0.7) +
  geom_hline(yintercept = mean(gammas$gamma_wml, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey50") +
  labs(x = NULL, y = "Prima momentum (mensual)", title = "Fama-MacBeth: prima del momentum") +
  theme_minimal(base_size = 13)
ggsave("output/fig_05_fama_macbeth.png", p2, width = 8, height = 5, dpi = 300)
cat("\n[OK] output/fig_05_fama_macbeth.png\n")

write_csv(momentum_returns, "data/ds_momentum_ch05.csv")
write_csv(gammas, "data/ds_gammas_fm_ch05.csv")
cat("[OK] Capitulo 5 completado.\n")
