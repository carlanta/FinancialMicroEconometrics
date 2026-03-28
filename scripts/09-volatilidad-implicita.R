# =============================================================================
# Capitulo 9: Volatilidad Implicita y Superficies
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - VIX, SP500, TREASURY_10Y
# =============================================================================

library(tidyverse)
library(readxl)
library(zoo)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 9: Volatilidad Implicita\n")
cat("  VIX vs Volatilidad Realizada\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

vix <- desk_read("VIX") |> select(date, vix_close = close)
sp  <- desk_read("SP500") |> mutate(ret = c(NA, diff(log(close)))) |> filter(!is.na(ret))

# Volatilidad realizada rolling 22 dias (anualizada)
sp <- sp |>
  mutate(rv_22 = rollapply(ret^2, 22, sum, fill = NA, align = "right") * 252)

df_vp <- inner_join(sp |> select(date, ret, rv_22), vix, by = "date") |>
  drop_na() |>
  mutate(vix_var = (vix_close / 100)^2, vp = vix_var - rv_22)

cat("Observaciones:", nrow(df_vp), "\n")
cat("Prima de varianza media:", round(mean(df_vp$vp), 6), "\n")
cat("Positiva en media:", ifelse(mean(df_vp$vp) > 0, "SI", "NO"), "\n")

# =============================================================================
# 2. GRAFICO PRIMA DE VARIANZA
# =============================================================================

p1 <- ggplot(df_vp, aes(x = date, y = vp)) +
  geom_line(linewidth = 0.3, color = "#2c3e50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
  geom_hline(yintercept = mean(df_vp$vp), linetype = "dotted", color = "#2980b9") +
  labs(x = NULL, y = "Prima de varianza (anualizada)",
       title = "Prima de varianza S&P 500 (VIX^2 - RV)") +
  theme_minimal(base_size = 13)

ggsave("output/fig_09_prima_varianza.png", p1, width = 8, height = 5, dpi = 300)
cat("\n[OK] output/fig_09_prima_varianza.png\n")

# =============================================================================
# 3. VIX vs TREASURY 10Y
# =============================================================================

t10y <- desk_read("TREASURY_10Y") |> select(date, yield = close)
df_vt <- inner_join(vix, t10y, by = "date") |> drop_na()

p2 <- ggplot(df_vt, aes(x = date)) +
  geom_line(aes(y = vix_close, color = "VIX"), linewidth = 0.4) +
  geom_line(aes(y = yield * 5, color = "Treasury 10Y"), linewidth = 0.4) +
  scale_y_continuous(name = "VIX",
                     sec.axis = sec_axis(~ . / 5, name = "Yield Treasury 10Y (%)")) +
  scale_color_manual(values = c("VIX" = "#c0392b", "Treasury 10Y" = "#2c3e50")) +
  labs(x = NULL, color = NULL, title = "VIX vs Treasury 10Y (flight to quality)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("output/fig_09_vix_treasury.png", p2, width = 8, height = 5, dpi = 300)
cat("[OK] output/fig_09_vix_treasury.png\n")

write_csv(df_vp |> select(date, vix_close, rv_22, vp), "data/ds_prima_varianza_ch09.csv")
cat("[OK] Capitulo 9 completado.\n")
