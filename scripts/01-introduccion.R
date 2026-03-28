# =============================================================================
# Capitulo 1: Introduccion - Econometria y Mercados Financieros
# Econometria del Riesgo Financiero
# =============================================================================
#
# Autor: Carlos de Anta Puig
#        carlos@cwconsultores.com
#
# Datos: desk.carlosdeanta.net
#   - TEF_MC.xlsx  (Telefonica)
#   - IBEX.xlsx    (IBEX-35)
#   - SP500.xlsx   (S&P 500)
#
# =============================================================================

library(tidyverse)
library(readxl)
library(kableExtra)
library(moments)

source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 1: Introduccion\n")
cat("  Primer contacto con los datos\n")
cat("========================================\n\n")

# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================

cat("--- Datos disponibles ---\n")
print(desk_list())

tef  <- desk_read("TEF_MC")
ibex <- desk_read("IBEX")
sp   <- desk_read("SP500")

cat("\nTelefonica:", nrow(tef), "observaciones\n")
cat("IBEX-35:   ", nrow(ibex), "observaciones\n")
cat("S&P 500:   ", nrow(sp), "observaciones\n")

# =============================================================================
# 2. GRAFICO DE PRECIOS NORMALIZADOS
# =============================================================================

ibex_norm <- ibex |>
  mutate(ibex = close / first(close) * 100) |>
  select(date, ibex)

sp_norm <- sp |>
  mutate(sp500 = close / first(close) * 100) |>
  select(date, sp500)

df_indices <- inner_join(ibex_norm, sp_norm, by = "date") |>
  pivot_longer(cols = c(ibex, sp500),
               names_to = "indice", values_to = "valor")

p1 <- ggplot(df_indices, aes(x = date, y = valor, color = indice)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(
    values = c("ibex" = "#c0392b", "sp500" = "#2c3e50"),
    labels = c("IBEX-35", "S&P 500")
  ) +
  labs(x = NULL, y = "Indice (base 100)",
       title = "IBEX-35 vs S&P 500 (base 100 = inicio)",
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("output/fig_01_precios_indices.png", p1, width = 8, height = 5, dpi = 300)
cat("\n[OK] Grafico guardado: output/fig_01_precios_indices.png\n")

# =============================================================================
# 3. RENDIMIENTOS DE TELEFONICA
# =============================================================================

ret_tef <- desk_returns("TEF_MC", type = "log")

p2 <- ggplot(ret_tef, aes(x = date, y = return)) +
  geom_line(linewidth = 0.3, color = "#2c3e50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Rendimiento log diario",
       title = "Rendimientos logaritmicos diarios de Telefonica (TEF.MC)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("output/fig_01_rendimientos_tef.png", p2, width = 8, height = 4, dpi = 300)
cat("[OK] Grafico guardado: output/fig_01_rendimientos_tef.png\n")

# =============================================================================
# 4. TABLA RESUMEN
# =============================================================================

ret_ibex <- desk_returns("IBEX", type = "log")
ret_sp   <- desk_returns("SP500", type = "log")

resumen <- bind_rows(
  ret_tef  |> summarise(Activo = "TEF.MC", N = n(),
                         Media = mean(return), DE = sd(return),
                         Min = min(return), Max = max(return),
                         Asimetria = skewness(return),
                         Curtosis = kurtosis(return)),
  ret_ibex |> summarise(Activo = "IBEX-35", N = n(),
                         Media = mean(return), DE = sd(return),
                         Min = min(return), Max = max(return),
                         Asimetria = skewness(return),
                         Curtosis = kurtosis(return)),
  ret_sp   |> summarise(Activo = "S&P 500", N = n(),
                         Media = mean(return), DE = sd(return),
                         Min = min(return), Max = max(return),
                         Asimetria = skewness(return),
                         Curtosis = kurtosis(return))
)

cat("\n--- Estadisticos descriptivos ---\n")
print(resumen |> mutate(across(where(is.numeric) & !matches("N"), ~round(., 4))))

# =============================================================================
# 5. GUARDAR DATASET
# =============================================================================

write_csv(resumen, "data/ds_resumen_ch01.csv")
cat("\n[OK] Dataset guardado: data/ds_resumen_ch01.csv\n")
cat("[OK] Capitulo 1 completado.\n")
