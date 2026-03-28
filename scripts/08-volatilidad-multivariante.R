# =============================================================================
# Capitulo 8: Volatilidad Multivariante y Correlaciones Dinamicas
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - IBEX, DAX, SP500, ORO
# =============================================================================

library(tidyverse)
library(readxl)
library(rugarch)
library(rmgarch)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 8: Volatilidad Multivariante\n")
cat("  DCC-GARCH IBEX/DAX/SP500\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

ibex <- desk_returns("IBEX", type = "log")
dax  <- desk_returns("DAX", type = "log")
sp   <- desk_returns("SP500", type = "log")

df3 <- inner_join(ibex |> rename(IBEX = return), dax |> rename(DAX = return), by = "date") |>
  inner_join(sp |> rename(SP500 = return), by = "date") |> drop_na()

cat("Observaciones alineadas:", nrow(df3), "\n")

# =============================================================================
# 2. DCC-GARCH
# =============================================================================

uspec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
mspec <- multispec(replicate(3, uspec))
dcc_spec <- dccspec(uspec = mspec, dccOrder = c(1,1), distribution = "mvt")

mat <- as.matrix(df3 |> select(IBEX, DAX, SP500))
dcc_fit <- dccfit(dcc_spec, data = mat)
cat("DCC-GARCH estimado\n")

# =============================================================================
# 3. CORRELACIONES DINAMICAS
# =============================================================================

R_t <- rcor(dcc_fit)
df_cor <- tibble(
  date = df3$date,
  `IBEX-DAX` = R_t[1, 2, ],
  `IBEX-SP500` = R_t[1, 3, ],
  `DAX-SP500` = R_t[2, 3, ]
) |> pivot_longer(-date, names_to = "par", values_to = "correlacion")

p1 <- ggplot(df_cor, aes(x = date, y = correlacion, color = par)) +
  geom_line(linewidth = 0.4, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  labs(x = NULL, y = "Correlacion condicional", color = NULL,
       title = "Correlaciones dinamicas DCC-GARCH") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("output/fig_08_dcc_correlaciones.png", p1, width = 8, height = 6, dpi = 300)
cat("[OK] output/fig_08_dcc_correlaciones.png\n")

# =============================================================================
# 4. IBEX-ORO (REFUGIO)
# =============================================================================

oro <- desk_returns("ORO", type = "log")
df_io <- inner_join(ibex |> rename(IBEX = return), oro |> rename(ORO = return), by = "date") |> drop_na()

uspec2 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
mspec2 <- multispec(replicate(2, uspec2))
dcc2 <- dccspec(uspec = mspec2, dccOrder = c(1,1), distribution = "mvt")
dcc2_fit <- dccfit(dcc2, data = as.matrix(df_io |> select(IBEX, ORO)))

R2 <- rcor(dcc2_fit)
df_cor2 <- tibble(date = df_io$date, cor_ibex_oro = R2[1,2,])

p2 <- ggplot(df_cor2, aes(x = date, y = cor_ibex_oro)) +
  geom_line(color = "#d4ac0d", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = NULL, y = "Correlacion IBEX-Oro", title = "Oro como activo refugio") +
  theme_minimal(base_size = 13)

ggsave("output/fig_08_dcc_oro.png", p2, width = 8, height = 4, dpi = 300)
cat("[OK] output/fig_08_dcc_oro.png\n")
cat("[OK] Capitulo 8 completado.\n")
