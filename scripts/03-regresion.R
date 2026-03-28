# =============================================================================
# Capitulo 3: Regresion Lineal en Contexto Financiero
# Econometria del Riesgo Financiero
# =============================================================================
#
# Autor: Carlos de Anta Puig
#        carlos@cwconsultores.com
#
# Datos: desk.carlosdeanta.net
#   - TEF_MC, ITX_MC, SAN_MC, IBE_MC, BBVA_MC, IBEX
#
# =============================================================================

library(tidyverse)
library(readxl)
library(kableExtra)
library(sandwich)
library(lmtest)

source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 3: Regresion Lineal\n")
cat("  Modelo de mercado (5 acciones IBEX)\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

tickers <- c("TEF_MC", "ITX_MC", "SAN_MC", "IBE_MC", "BBVA_MC")
nombres <- c("Telefonica", "Inditex", "Santander", "Iberdrola", "BBVA")

ret_ibex <- desk_returns("IBEX", type = "log") |> rename(r_m = return)

datos <- map(tickers, function(tk) {
  desk_returns(tk, type = "log") |>
    rename(r_i = return) |>
    inner_join(ret_ibex, by = "date")
})
names(datos) <- nombres

cat("Observaciones por accion:\n")
print(map_int(datos, nrow))

# =============================================================================
# 2. ESTIMACION MCO
# =============================================================================

modelos <- map(datos, ~ lm(r_i ~ r_m, data = .x))

# =============================================================================
# 3. ERRORES ESTANDAR: CLASICOS vs HC3 vs NEWEY-WEST
# =============================================================================

cat("\n--- Comparacion de errores estandar (beta) ---\n")
for (nm in nombres) {
  m <- modelos[[nm]]
  ee_c  <- sqrt(diag(vcov(m)))[2]
  ee_h  <- sqrt(diag(vcovHC(m, type = "HC3")))[2]
  ee_nw <- sqrt(diag(vcovHAC(m)))[2]
  cat(sprintf("%-12s Beta=%.3f  Clasico=%.4f  HC3=%.4f  NW=%.4f  Ratio=%.2f\n",
              nm, coef(m)[2], ee_c, ee_h, ee_nw, ee_h/ee_c))
}

# =============================================================================
# 4. SCATTER PLOTS
# =============================================================================

df_scatter <- map_dfr(nombres, function(nm) {
  datos[[nm]] |> mutate(accion = nm)
})

p1 <- ggplot(df_scatter, aes(x = r_m, y = r_i)) +
  geom_point(alpha = 0.15, size = 0.5, color = "#2980b9") +
  geom_smooth(method = "lm", se = FALSE, color = "#c0392b", linewidth = 0.8) +
  facet_wrap(~accion, scales = "free_y") +
  labs(x = "Rendimiento IBEX-35", y = "Rendimiento accion") +
  theme_minimal(base_size = 12)

ggsave("output/fig_03_scatter_betas.png", p1, width = 8, height = 6, dpi = 300)
cat("\n[OK] output/fig_03_scatter_betas.png\n")

# =============================================================================
# 5. BREUSCH-PAGAN
# =============================================================================

cat("\n--- Test de Breusch-Pagan ---\n")
for (nm in nombres) {
  bp <- bptest(modelos[[nm]])
  cat(sprintf("%-12s BP stat=%.2f  p=%.4f\n", nm, bp$statistic, bp$p.value))
}

# =============================================================================
# 6. TABLA RESUMEN TIPO PAPER
# =============================================================================

cat("\n--- Tabla resumen (errores HC3) ---\n")
tabla <- map_dfr(nombres, function(nm) {
  m <- modelos[[nm]]
  ee <- sqrt(diag(vcovHC(m, type = "HC3")))
  s <- summary(m)
  tibble(Accion = nm, Alpha = round(coef(m)[1], 4),
         Beta = round(coef(m)[2], 3), EE_beta = round(ee[2], 4),
         R2 = round(s$r.squared, 3), N = nrow(datos[[nm]]))
})
print(tabla)

write_csv(tabla, "data/ds_modelo_mercado_ch03.csv")
cat("\n[OK] Capitulo 3 completado.\n")
