# =============================================================================
# Capitulo 7: Modelos de Volatilidad Condicional - GARCH Univariante
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - EURUSD.xlsx
# =============================================================================

library(tidyverse)
library(readxl)
library(rugarch)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 7: GARCH Univariante\n")
cat("  Activo: EUR/USD\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

ret_eur <- desk_returns("EURUSD", type = "log")
r <- ret_eur$return
cat("EUR/USD:", length(r), "rendimientos diarios\n")

# =============================================================================
# 2. ESTIMACION DE 4 MODELOS
# =============================================================================

specs <- list(
  "GARCH-N" = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0)), distribution.model = "norm"),
  "GARCH-t" = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0)), distribution.model = "std"),
  "GJR-t"   = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0)), distribution.model = "std"),
  "EGARCH-t" = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
)

fits <- map(specs, ~ ugarchfit(.x, data = r, solver = "hybrid"))

# =============================================================================
# 3. TABLA COMPARATIVA
# =============================================================================

comp <- map_dfr(names(fits), function(nm) {
  f <- fits[[nm]]; ic <- infocriteria(f)
  tibble(Modelo = nm, AIC = ic[1], BIC = ic[2], LogLik = likelihood(f))
})

cat("\n--- Comparacion de modelos ---\n")
print(comp |> mutate(across(where(is.numeric), ~round(., 4))))

# =============================================================================
# 4. VOLATILIDAD CONDICIONAL
# =============================================================================

vol_gjr <- sigma(fits[["GJR-t"]])
df_vol <- tibble(date = ret_eur$date, vol = as.numeric(vol_gjr) * 100)

p1 <- ggplot(df_vol, aes(x = date, y = vol)) +
  geom_line(color = "#c0392b", linewidth = 0.3, alpha = 0.8) +
  labs(x = NULL, y = "Volatilidad condicional (% diario)",
       title = "GJR-GARCH(1,1) t-Student - EUR/USD") +
  theme_minimal(base_size = 13)

ggsave("output/fig_07_vol_eurusd.png", p1, width = 8, height = 4, dpi = 300)
cat("\n[OK] output/fig_07_vol_eurusd.png\n")

# =============================================================================
# 5. NEWS IMPACT CURVE
# =============================================================================

ni_sym <- newsimpact(fits[["GARCH-t"]])
ni_gjr <- newsimpact(fits[["GJR-t"]])
df_ni <- bind_rows(
  tibble(z = ni_sym$zx, sigma2 = ni_sym$zy, modelo = "GARCH simetrico"),
  tibble(z = ni_gjr$zx, sigma2 = ni_gjr$zy, modelo = "GJR-GARCH")
)

p2 <- ggplot(df_ni, aes(x = z, y = sigma2, color = modelo)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("GARCH simetrico" = "#2980b9", "GJR-GARCH" = "#c0392b")) +
  labs(x = "Shock (epsilon)", y = "Varianza condicional", color = NULL,
       title = "News Impact Curve") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("output/fig_07_news_impact.png", p2, width = 8, height = 5, dpi = 300)
cat("[OK] output/fig_07_news_impact.png\n")

write_csv(comp, "data/ds_garch_comp_ch07.csv")
cat("[OK] Capitulo 7 completado.\n")
