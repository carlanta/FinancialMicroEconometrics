# =============================================================================
# Capitulo 6: Estudios de Eventos (AMPLIADO)
# Econometria del Riesgo Financiero
# =============================================================================
# Autor: Carlos de Anta Puig | carlos@cwconsultores.com
# Datos: desk.carlosdeanta.net - IBEX35_bulk_close.csv
# Eventos: COVID, Lehman, Brexit
# =============================================================================

library(tidyverse)
library(readxl)
library(kableExtra)
source("scripts/deskR.R")

cat("\n========================================\n")
cat("  Capitulo 6: Event Studies (ampliado)\n")
cat("========================================\n\n")

# === 1. DATOS ===
ibex_bulk <- read_csv("data/IBEX35_bulk_close.csv", show_col_types = FALSE) |> mutate(date = as.Date(date))
idx_col <- "^IBEX"
acciones <- setdiff(names(ibex_bulk), c("date", idx_col))

ret_bulk <- ibex_bulk |> arrange(date) |>
  mutate(across(-date, ~ c(NA, diff(log(.))))) |> slice(-1)

# === 2. FUNCION EVENT STUDY ===
run_event_study <- function(ret_bulk, acciones, idx_col, event_date,
                            est_start=-260, est_end=-11, evt_start=-5, evt_end=+10) {
  trading_dates <- sort(ret_bulk$date)
  event_idx <- which.min(abs(trading_dates - event_date))
  event_actual <- trading_dates[event_idx]

  map_dfr(acciones, function(acc) {
    df <- ret_bulk |> select(date, r_i=all_of(acc), r_m=all_of(idx_col)) |> drop_na() |> arrange(date)
    ei <- which(df$date == event_actual)
    if(length(ei)==0 || ei+est_start<1 || ei+evt_end>nrow(df)) return(NULL)
    est <- df[(ei+est_start):(ei+est_end),]
    m <- lm(r_i ~ r_m, data=est)
    evt <- df[(ei+evt_start):(ei+evt_end),] |>
      mutate(r_normal=predict(m, newdata=pick(everything())), AR=r_i-r_normal, day=evt_start:evt_end)
    evt |> select(date, day, AR) |> mutate(ticker=acc, sigma=summary(m)$sigma)
  })
}

# === 3. COVID ===
cat("--- COVID (11/03/2020) ---\n")
event_results <- run_event_study(ret_bulk, acciones, idx_col, as.Date("2020-03-11"))
car_covid <- event_results |>
  group_by(day) |> summarise(AR=mean(AR,na.rm=TRUE), .groups="drop") |> mutate(CAR=cumsum(AR))

p1 <- ggplot(car_covid, aes(day, CAR)) +
  geom_line(color="#2c3e50", linewidth=0.8) + geom_point(color="#2c3e50", size=2) +
  geom_vline(xintercept=0, linetype="dashed", color="#c0392b") +
  geom_hline(yintercept=0, linetype="dotted") +
  labs(x="Dias", y="CAR medio", title="COVID-19 (11/03/2020)") + theme_minimal()
ggsave("output/fig_06_car_covid.png", p1, width=8, height=5, dpi=300)
cat("[OK] fig_06_car_covid.png\n")

# === 4. COMPARACION: COVID vs LEHMAN vs BREXIT ===
cat("\n--- Comparacion multi-evento ---\n")
eventos <- list("Lehman"=as.Date("2008-09-15"), "Brexit"=as.Date("2016-06-24"), "COVID"=as.Date("2020-03-11"))

car_comp <- map_dfr(names(eventos), function(nm) {
  res <- run_event_study(ret_bulk, acciones, idx_col, eventos[[nm]])
  if(is.null(res)||nrow(res)==0) return(NULL)
  res |> group_by(day) |> summarise(AR=mean(AR,na.rm=TRUE), .groups="drop") |>
    mutate(CAR=cumsum(AR), evento=nm)
})

p2 <- ggplot(car_comp, aes(day, CAR, color=evento)) +
  geom_line(linewidth=0.8) + geom_point(size=1.5) +
  geom_vline(xintercept=0, linetype="dashed", color="grey50") +
  geom_hline(yintercept=0, linetype="dotted") +
  scale_color_manual(values=c("#c0392b","#2980b9","#2c3e50")) +
  labs(x="Dias", y="CAR medio", color=NULL, title="CAR comparativo: Lehman vs Brexit vs COVID") +
  theme_minimal() + theme(legend.position="bottom")
ggsave("output/fig_06_car_comparativo.png", p2, width=8, height=5, dpi=300)
cat("[OK] fig_06_car_comparativo.png\n")

# === 5. ANALISIS POR SECTORES (COVID) ===
cat("\n--- Sectores (COVID) ---\n")
sectores <- list(
  Banca=c("SAN.MC","BBVA.MC","CABK.MC","SAB.MC","BKT.MC"),
  Utilities=c("IBE.MC","ELE.MC","NTGY.MC","REP.MC"),
  Consumo=c("ITX.MC","GRF.MC"))

car_sec <- map_dfr(names(sectores), function(sec) {
  accs <- intersect(sectores[[sec]], acciones)
  if(length(accs)==0) return(NULL)
  res <- run_event_study(ret_bulk, accs, idx_col, as.Date("2020-03-11"))
  if(is.null(res)||nrow(res)==0) return(NULL)
  res |> group_by(day) |> summarise(AR=mean(AR,na.rm=TRUE), .groups="drop") |>
    mutate(CAR=cumsum(AR), sector=sec)
})

if(nrow(car_sec)>0) {
  p3 <- ggplot(car_sec, aes(day, CAR, color=sector)) +
    geom_line(linewidth=0.8) + geom_point(size=1.5) +
    geom_vline(xintercept=0, linetype="dashed") + geom_hline(yintercept=0, linetype="dotted") +
    scale_color_manual(values=c(Banca="#c0392b",Utilities="#27ae60",Consumo="#2980b9")) +
    labs(x="Dias", y="CAR medio", color="Sector", title="CAR por sector (COVID)") +
    theme_minimal() + theme(legend.position="bottom")
  ggsave("output/fig_06_car_sectorial.png", p3, width=8, height=5, dpi=300)
  cat("[OK] fig_06_car_sectorial.png\n")
}

# === 6. CAR INDIVIDUAL ===
cat("\n--- CAR individual [-1,+5] COVID ---\n")
car_indiv <- event_results |> filter(day>=-1, day<=5) |>
  group_by(ticker) |> summarise(CAR=sum(AR), .groups="drop") |> arrange(CAR)
print(car_indiv |> mutate(CAR_pct=sprintf("%.2f%%", CAR*100)))

write_csv(car_covid, "data/ds_car_covid_ch06.csv")
write_csv(car_comp, "data/ds_car_comparativo_ch06.csv")
cat("\n[OK] Capitulo 6 completado.\n")
