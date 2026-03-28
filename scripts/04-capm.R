# =============================================================================
# Capitulo 4: El CAPM y su Contrastacion Empirica
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net
#   - IBEX35_bulk_close.csv (30 acciones + indice)
#   - TREASURY_13W.xlsx
# =============================================================================

library(tidyverse)
library(readxl)
library(sandwich)
library(lmtest)
library(kableExtra)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 4: CAPM\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

ibex_bulk <- read_csv("data/IBEX35_bulk_close.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date))

idx_col <- "^IBEX"
acciones <- setdiff(names(ibex_bulk), c("date", idx_col))
cat("Acciones IBEX:", length(acciones), "\n")

# Rendimientos
ret_bulk <- ibex_bulk |>
  arrange(date) |>
  mutate(across(-date, ~ c(NA, diff(log(.))))) |>
  slice(-1)

# Tipo libre de riesgo
rf_data <- desk_read("TREASURY_13W") |>
  mutate(rf_daily = close / 100 / 252) |>
  select(date, rf_daily)

ret_data <- ret_bulk |>
  inner_join(rf_data, by = "date") |>
  drop_na()

# Excesos de rendimiento
ret_excess <- ret_data |>
  mutate(across(all_of(c(acciones, idx_col)), ~ . - rf_daily))

cat("Observaciones con rf:", nrow(ret_excess), "\n")

# =============================================================================
# 2. ESTIMACION CAPM: 30 REGRESIONES
# =============================================================================

resultados <- map_dfr(acciones, function(acc) {
  df <- ret_excess |>
    select(date, r_i = all_of(acc), r_m = all_of(idx_col)) |>
    drop_na()
  if (nrow(df) < 252) return(NULL)
  m <- lm(r_i ~ r_m, data = df)
  ee_hc3 <- sqrt(diag(vcovHC(m, type = "HC3")))
  s <- summary(m)
  t_alpha <- coef(m)[1] / ee_hc3[1]
  p_alpha <- 2 * pt(-abs(t_alpha), df = m$df.residual)
  tibble(Ticker = acc, Alpha = coef(m)[1], Beta = coef(m)[2],
         R2 = s$r.squared, p_alpha = p_alpha, N = nrow(df))
})

cat("\n--- Resultados CAPM ---\n")
cat("Acciones estimadas:", nrow(resultados), "\n")
cat("Alfas significativos (5%):", sum(resultados$p_alpha < 0.05), "\n\n")
print(resultados |> mutate(across(c(Alpha, Beta, R2), ~round(., 4))))

# =============================================================================
# 3. SECURITY MARKET LINE
# =============================================================================

ret_medios <- map_dfr(acciones, function(acc) {
  r <- ret_excess[[acc]]; r <- r[!is.na(r)]
  tibble(Ticker = acc, ret_anual = mean(r) * 252)
})
sml_df <- resultados |> inner_join(ret_medios, by = "Ticker")
prima_mercado <- mean(ret_excess[[idx_col]], na.rm = TRUE) * 252

p1 <- ggplot(sml_df, aes(x = Beta, y = ret_anual)) +
  geom_abline(intercept = 0, slope = prima_mercado,
              color = "#c0392b", linewidth = 0.8, linetype = "dashed") +
  geom_point(color = "#2c3e50", size = 3) +
  geom_text(aes(label = Ticker), hjust = -0.15, vjust = -0.3, size = 2.5) +
  labs(x = "Beta", y = "Rendimiento medio anualizado en exceso") +
  theme_minimal(base_size = 13)

ggsave("output/fig_04_sml.png", p1, width = 8, height = 6, dpi = 300)
cat("[OK] output/fig_04_sml.png\n")

# =============================================================================
# 4. BETAS ROLLING
# =============================================================================

sel <- c("SAN.MC", "BBVA.MC", "IBE.MC", "ITX.MC")
sel_nombres <- c("Santander", "BBVA", "Iberdrola", "Inditex")
ventana <- 252

rolling_betas <- map_dfr(seq_along(sel), function(i) {
  df <- ret_excess |>
    select(date, r_i = all_of(sel[i]), r_m = all_of(idx_col)) |>
    drop_na() |> arrange(date)
  if (nrow(df) < ventana + 10) return(NULL)
  betas <- map_dbl(ventana:nrow(df), function(j) {
    coef(lm(r_i ~ r_m, data = df[(j - ventana + 1):j, ]))[2]
  })
  tibble(date = df$date[ventana:nrow(df)], beta = betas, accion = sel_nombres[i])
})

p2 <- ggplot(rolling_betas, aes(x = date, y = beta, color = accion)) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Beta rolling (252 dias)", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("output/fig_04_betas_rolling.png", p2, width = 8, height = 6, dpi = 300)
cat("[OK] output/fig_04_betas_rolling.png\n")

write_csv(resultados, "data/ds_capm_ch04.csv")
cat("\n[OK] Capitulo 4 completado.\n")
