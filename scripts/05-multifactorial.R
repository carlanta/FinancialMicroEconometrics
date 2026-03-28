# =============================================================================
# Capitulo 5: Modelos Multifactoriales (AMPLIADO)
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

# === 1. DATOS ===
sp_bulk <- read_csv("data/SP500_bulk_close.csv", show_col_types = FALSE) |> mutate(date = as.Date(date))
idx_sp <- "^GSPC"
acciones_sp <- setdiff(names(sp_bulk), c("date", idx_sp))
cat("Acciones S&P 500:", length(acciones_sp), "\n")

sp_monthly <- sp_bulk |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month) |>
  summarise(across(-date, ~ { v <- na.omit(.); if(length(v)<2) NA_real_ else last(v)/first(v)-1 }), .groups = "drop")
meses <- sort(unique(sp_monthly$month))

# === 2. FACTOR MOMENTUM ===
momentum_returns <- map_dfr(13:length(meses), function(i) {
  past_data <- sp_monthly |> filter(month %in% meses[(i-12):(i-1)])
  cum_ret <- past_data |> summarise(across(all_of(acciones_sp), ~ prod(1+., na.rm=TRUE)-1))
  cv <- unlist(cum_ret[1,]); cv <- cv[!is.na(cv) & is.finite(cv)]
  if(length(cv)<20) return(NULL)
  q <- quantile(cv, c(0.2,0.8)); w <- names(cv[cv>=q[2]]); l <- names(cv[cv<=q[1]])
  cur <- sp_monthly |> filter(month==meses[i])
  tibble(date=meses[i], WML=mean(unlist(cur[1,w]),na.rm=TRUE)-mean(unlist(cur[1,l]),na.rm=TRUE))
})
cat("Momentum:", nrow(momentum_returns), "meses, media:", round(mean(momentum_returns$WML)*100,2), "%\n")

p1 <- momentum_returns |> mutate(cum=cumprod(1+WML)-1) |>
  ggplot(aes(date, cum)) + geom_line(color="#2c3e50") + geom_hline(yintercept=0, linetype="dashed") +
  labs(x=NULL, y="Rend. acumulado", title="Momentum (Q5-Q1) S&P 500") + theme_minimal()
ggsave("output/fig_05_momentum.png", p1, width=8, height=5, dpi=300)

# === 3. FACTOR SMB ===
size_proxy <- sp_bulk |> mutate(month=floor_date(date,"month")) |>
  group_by(month) |> summarise(across(all_of(acciones_sp), ~{v<-na.omit(.);if(length(v)<10)NA_real_ else mean(v)}), .groups="drop")

smb_returns <- map_dfr(2:nrow(sp_monthly), function(i) {
  if(i>nrow(size_proxy)) return(NULL)
  sz <- unlist(size_proxy[i-1,acciones_sp]); sz <- sz[!is.na(sz)&is.finite(sz)&sz>0]
  if(length(sz)<20) return(NULL)
  med <- median(sz); sm <- names(sz[sz<=med]); bg <- names(sz[sz>med])
  ret <- unlist(sp_monthly[i,acciones_sp])
  tibble(date=sp_monthly$month[i], SMB=mean(ret[sm],na.rm=TRUE)-mean(ret[bg],na.rm=TRUE))
})
cat("SMB:", nrow(smb_returns), "meses, media:", round(mean(smb_returns$SMB,na.rm=TRUE)*100,2), "%\n")

# === 4. QUINTILES DE MOMENTUM ===
quintil_returns <- map_dfr(13:length(meses), function(i) {
  past_data <- sp_monthly |> filter(month %in% meses[(i-12):(i-1)])
  cv <- unlist(past_data |> summarise(across(all_of(acciones_sp), ~prod(1+.,na.rm=TRUE)-1)))
  cv <- cv[!is.na(cv)&is.finite(cv)]; if(length(cv)<20) return(NULL)
  q <- quantile(cv, seq(0,1,0.2)); cur <- sp_monthly |> filter(month==meses[i])
  map_dfr(1:5, function(qi) {
    sel <- if(qi==1) names(cv[cv<=q[2]]) else if(qi==5) names(cv[cv>=q[5]]) else names(cv[cv>q[qi]&cv<=q[qi+1]])
    tibble(date=meses[i], quintil=qi, ret=mean(unlist(cur[1,sel]),na.rm=TRUE))
  })
})
qm <- quintil_returns |> group_by(quintil) |> summarise(ret_medio=mean(ret,na.rm=TRUE)*100)
p2 <- ggplot(qm, aes(factor(quintil), ret_medio, fill=factor(quintil))) +
  geom_col(alpha=0.8) + scale_fill_manual(values=c("#c0392b","#e67e22","#f1c40f","#27ae60","#2c3e50")) +
  labs(x="Quintil momentum", y="Rend. medio mensual (%)") + theme_minimal() + theme(legend.position="none")
ggsave("output/fig_05_quintiles.png", p2, width=8, height=5, dpi=300)
cat("[OK] fig_05_quintiles.png\n")

# === 5. REGRESIONES CAPM vs 2F vs 3F ===
sp_idx_m <- sp_monthly |> select(month, mkt=all_of(idx_sp)) |> drop_na()
acc_val <- acciones_sp[map_lgl(acciones_sp, ~sum(!is.na(sp_monthly[[.x]]))>80)]
set.seed(42); acc_sel <- sample(acc_val, min(20, length(acc_val)))

df_3f <- sp_monthly |> select(month, all_of(acc_sel)) |>
  inner_join(sp_idx_m, by="month") |>
  inner_join(momentum_returns |> rename(month=date), by="month") |>
  inner_join(smb_returns |> rename(month=date), by="month")

comp <- map_dfr(acc_sel, function(tk) {
  df <- df_3f |> select(month, r=all_of(tk), mkt, WML, SMB) |> drop_na()
  if(nrow(df)<24) return(NULL)
  tibble(Ticker=tk, R2_1F=summary(lm(r~mkt,df))$r.squared,
         R2_2F=summary(lm(r~mkt+WML,df))$r.squared,
         R2_3F=summary(lm(r~mkt+WML+SMB,df))$r.squared)
})
cat("\nR2 medio: CAPM=", round(mean(comp$R2_1F),3), " 2F=", round(mean(comp$R2_2F),3),
    " 3F=", round(mean(comp$R2_3F),3), "\n")

# === 6. FAMA-MACBETH ===
cat("\n--- Fama-MacBeth ---\n")
meses_ord <- sort(unique(df_3f$month))
m1 <- meses_ord[1:min(36, floor(length(meses_ord)/2))]
m2 <- meses_ord[(length(m1)+1):length(meses_ord)]

betas <- map_dfr(acc_sel, function(tk) {
  df <- df_3f |> filter(month%in%m1) |> select(r=all_of(tk),mkt,WML) |> drop_na()
  if(nrow(df)<12) return(NULL)
  m <- lm(r~mkt+WML,df); tibble(Ticker=tk, beta_mkt=coef(m)["mkt"], beta_wml=coef(m)["WML"])
})

gammas <- map_dfr(m2, function(mes) {
  ret <- df_3f |> filter(month==mes) |> select(all_of(acc_sel)) |>
    pivot_longer(everything(), names_to="Ticker", values_to="r") |> drop_na()
  cs <- inner_join(ret, betas, by="Ticker"); if(nrow(cs)<5) return(NULL)
  m <- lm(r~beta_mkt+beta_wml, cs)
  tibble(month=mes, gamma_0=coef(m)[1], gamma_mkt=coef(m)["beta_mkt"], gamma_wml=coef(m)["beta_wml"])
})

for(col in c("gamma_0","gamma_mkt","gamma_wml")) {
  v <- na.omit(gammas[[col]]); m <- mean(v); se <- sd(v)/sqrt(length(v))
  cat(sprintf("  %-12s Media=%.4f  t=%.3f\n", c(gamma_0="Intercepto",gamma_mkt="Mercado",gamma_wml="Momentum")[col], m, m/se))
}

p3 <- ggplot(gammas, aes(month, gamma_wml)) +
  geom_col(fill=ifelse(gammas$gamma_wml>0,"#2980b9","#c0392b"), alpha=0.7) +
  geom_hline(yintercept=mean(gammas$gamma_wml,na.rm=TRUE), linetype="dashed") +
  labs(x=NULL, y="Prima momentum", title="Fama-MacBeth") + theme_minimal()
ggsave("output/fig_05_fama_macbeth.png", p3, width=8, height=5, dpi=300)

write_csv(momentum_returns, "data/ds_momentum_ch05.csv")
write_csv(gammas, "data/ds_gammas_fm_ch05.csv")
cat("\n[OK] Capitulo 5 completado.\n")
