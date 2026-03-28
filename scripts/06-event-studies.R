# =============================================================================
# Capitulo 6: Estudios de Eventos
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - IBEX35_bulk_close.csv
# Evento: Declaracion de pandemia OMS, 11 marzo 2020
# =============================================================================

library(tidyverse)
library(readxl)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 6: Event Studies\n")
cat("  Evento: COVID-19 (11/03/2020)\n")
cat("========================================\n\n")

# =============================================================================
# 1. DATOS
# =============================================================================

ibex_bulk <- read_csv("data/IBEX35_bulk_close.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date))
idx_col <- "^IBEX"
acciones <- setdiff(names(ibex_bulk), c("date", idx_col))

ret_bulk <- ibex_bulk |>
  arrange(date) |>
  mutate(across(-date, ~ c(NA, diff(log(.))))) |>
  slice(-1)

event_date <- as.Date("2020-03-11")
trading_dates <- sort(ret_bulk$date)
event_idx <- which.min(abs(trading_dates - event_date))
event_actual <- trading_dates[event_idx]
cat("Fecha del evento:", as.character(event_actual), "\n")

# =============================================================================
# 2. EVENT STUDY
# =============================================================================

est_start <- -260; est_end <- -11
evt_start <- -5; evt_end <- +10

event_results <- map_dfr(acciones, function(acc) {
  df <- ret_bulk |>
    select(date, r_i = all_of(acc), r_m = all_of(idx_col)) |>
    drop_na() |> arrange(date)
  evt_idx <- which(df$date == event_actual)
  if (length(evt_idx) == 0) return(NULL)
  if (evt_idx + est_start < 1 || evt_idx + evt_end > nrow(df)) return(NULL)

  est_data <- df[(evt_idx + est_start):(evt_idx + est_end), ]
  m <- lm(r_i ~ r_m, data = est_data)

  evt_data <- df[(evt_idx + evt_start):(evt_idx + evt_end), ] |>
    mutate(r_normal = predict(m, newdata = pick(everything())),
           AR = r_i - r_normal, day = evt_start:evt_end)
  evt_data |> select(date, day, AR) |> mutate(ticker = acc, sigma = summary(m)$sigma)
})

cat("Acciones con event study:", n_distinct(event_results$ticker), "\n")

# =============================================================================
# 3. CAR MEDIO
# =============================================================================

car_medio <- event_results |>
  group_by(day) |>
  summarise(AR_medio = mean(AR, na.rm = TRUE),
            AR_se = sd(AR, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop") |>
  mutate(CAR = cumsum(AR_medio))

p1 <- ggplot(car_medio, aes(x = day, y = CAR)) +
  geom_ribbon(aes(ymin = CAR - 1.96 * AR_se * sqrt(abs(day) + 1),
                  ymax = CAR + 1.96 * AR_se * sqrt(abs(day) + 1)),
              fill = "#2980b9", alpha = 0.2) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  geom_point(color = "#2c3e50", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#c0392b") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  labs(x = "Dias relativos al evento", y = "CAR medio",
       title = "CAR medio IBEX-35 - COVID-19 (11/03/2020)") +
  theme_minimal(base_size = 13)

ggsave("output/fig_06_car_covid.png", p1, width = 8, height = 5, dpi = 300)
cat("[OK] output/fig_06_car_covid.png\n")

# =============================================================================
# 4. TESTS
# =============================================================================

cat("\n--- Tests por ventana ---\n")
for (w in list(c(-1,1), c(-1,5), c(0,5), c(0,10))) {
  sub <- event_results |>
    filter(day >= w[1], day <= w[2]) |>
    group_by(ticker) |> summarise(CAR = sum(AR), .groups = "drop")
  t_stat <- mean(sub$CAR) / (sd(sub$CAR) / sqrt(nrow(sub)))
  cat(sprintf("  [%+d, %+d] CAR=%.4f  t=%.3f  N=%d\n",
              w[1], w[2], mean(sub$CAR), t_stat, nrow(sub)))
}

write_csv(car_medio, "data/ds_car_covid_ch06.csv")
cat("\n[OK] Capitulo 6 completado.\n")
