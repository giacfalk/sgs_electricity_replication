
library(tidyverse)
library(sf)
library(raster)
library(exactextractr)

result <- read_rds("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/bilanci_italia/budgets_comuni_Tutela_valorizzazione_recupero_ambientale_2017_2023.rds")

result <- st_as_sf(result)
result <-  st_transform(result, 4326)

result <- filter(result, !st_is_empty(geometry))

###

sf::sf_use_s2(F)

area <- st_area(result$geometry)

pop <- raster("H:/ECIP/Falchetta/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif")

result$pop <- exact_extract(pop, result, "sum")

###

result$area <- as.numeric(area/1e6)

###

write_rds(result, "F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/bilanci_italia/budgets_comuni_Tutela_valorizzazione_recupero_ambientale_2017_2023_poparea.rds")

###

result <- read_rds("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/bilanci_italia/budgets_comuni_Tutela_valorizzazione_recupero_ambientale_2017_2023_poparea.rds")

result$geometry <- NULL

result <- filter(result, Desc.Titolo.Spese.Arconet %in% c("Spese correnti", "Spese in conto capitale"))
result <- filter(result, Descrizione.Tipologia.Soggetto=="COMUNI")

result_p <- filter(result, Tot..Pag.<1e7)

ggplot(result_p)+
  geom_histogram(aes(x=Tot..Pag./pop))+
  scale_x_continuous(limits = c(0.01, 100))+
  facet_wrap(vars(Desc.Titolo.Spese.Arconet))

###

ggplot(result_p)+
  geom_histogram(aes(x=Tot..Pag./area))+
  scale_x_continuous(limits = c(1, 10000))+
  facet_wrap(vars(Desc.Titolo.Spese.Arconet))

###

GVI_stats_comuni_2016_2023<-readRDS("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/GVI_stats_comuni_2016_2023.rds")

GVI_stats_comuni_2016_2023$geometry <- NULL

GVI_stats_comuni_2016_2023 <- dplyr::select(GVI_stats_comuni_2016_2023, -c(1:11, 13:15))
GVI_stats_comuni_2016_2023 <- dplyr::select(GVI_stats_comuni_2016_2023, COMUNE, contains("mean"))

GVI_stats_comuni_2016_2023 <- GVI_stats_comuni_2016_2023 %>%
  pivot_longer(
    cols = starts_with("GVI_mean_"),         # Columns to pivot
    names_to = "year",                        # New column for year names
    names_prefix = "GVI_mean_",               # Remove this prefix from names
    values_to = "GVI_mean"                    # New column for values
  )

results <- merge(result, GVI_stats_comuni_2016_2023, by.x=c("COMUNE.y", "Esercizio.Finanziario"), by.y=c("COMUNE", "year"))

ggplot(results)+
  geom_histogram(aes(x=Tot..Pag. / (area*GVI_mean/100)))+
  scale_x_continuous(limits = c(1, 50000))+
  facet_wrap(vars(Desc.Titolo.Spese.Arconet))

###

ggplot(results)+
  geom_histogram(aes(x=Tot..Pag. / (GVI_mean/100)))+
  scale_x_continuous(limits = c(1, 5e5))+
  facet_wrap(vars(Desc.Titolo.Spese.Arconet))

###

# una regressione che derivi elasticità alla spesa (delta spesa) all gvi

results$pagkm2 <- results$Tot..Pag./results$area
results$pagpop <- results$Tot..Pag./results$pop

ggplot(results)+
  geom_histogram(aes(x=pagkm2))+
  scale_x_continuous(limits = c(1, 10000))+
  facet_wrap(vars(Desc.Titolo.Spese.Arconet))

# result <- filter(result, Tot..Pag.<5e5 & Tot..Pag.>0)

results$Tot..Pag._mill <- results$Tot..Pag./1e6

###

formula <- as.formula("GVI_mean  ~ Tot..Pag._mill : Desc.Titolo.Spese.Arconet | COMUNE.y +  Esercizio.Finanziario")


library(fixest)

mult1 = feols(formula, data = results)
etable(mult1, vcov = "cluster")

etable(mult1, vcov="cluster")
etable(mult1, vcov="cluster", file = paste0("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/", "exp_inv_reg.tex"), tex = T, replace = T)

####

df <- read_rds("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/processed_data.rds")
df <- df[df$cons_719_month>50,]

####
# across the municipalities covered by the electricity analysis
results_p <- df  %>% dplyr::group_by(comune) %>% dplyr::summarise(GVI_mean=mean(gvi_mean, na.rm=T))

###

table(21 - results_p$GVI_mean>0)

# quanti milioni di investimento servono per portare GVI a 21 in ciascuna municipalità?
rr <- ifelse(21 - results_p$GVI_mean<0, 0, 21 - results_p$GVI_mean) / sum(coef(mult1)) 

summary(rr)

# scontandoli al 3% per 25 annni
library(FinCal)
rr_npv <- unlist(lapply(rr, function(X){npv(0.03, rep(X/25, 25))}))

sum(rr_npv)/1e3  # NPV

# calcolando il discounted average yearly cost

eaa_from_npv <- function(npv, r, T) {
  npv * r / (1 - (1 + r)^(-T))
}

rr_eea <- unlist(lapply(rr_npv, function(X){eaa_from_npv(X, 0.03, 25)}))

sum(rr_eea)/1e3  # EEA

####
####
####

# across all italian municipalities
results_p <- GVI_stats_comuni_2016_2023  %>% dplyr::group_by(COMUNE) %>% dplyr::summarise(GVI_mean=mean(GVI_mean, na.rm=T))

###

table(21 - results_p$GVI_mean>0)

# quanti milioni di investimento servono per portare GVI a 21 in ciascuna municipalità?
rr <- ifelse(21 - results_p$GVI_mean<0, 0, 21 - results_p$GVI_mean) / sum(coef(mult1)) 

summary(rr)

# scontandoli al 3% per 25 annni
library(FinCal)
rr_npv <- unlist(lapply(rr, function(X){npv(0.03, rep(X/25, 25))}))

sum(rr_npv)/1e3  # NPV

# calcolando il discounted average yearly cost

eaa_from_npv <- function(npv, r, T) {
  npv * r / (1 - (1 + r)^(-T))
}

rr_eea <- unlist(lapply(rr_npv, function(X){eaa_from_npv(X, 0.03, 25)}))

sum(rr_eea)/1e3  # EEA

summary(rr_eea[rr_eea>0]*1e3)

png("fig_hist_inv_needed.png", width=6, height=4, units="in", res=300)
hist(rr_eea[rr_eea>0]*1e3, main="Equivalent Annual Annuity (EEA) cost per municipality", xlab="Thousand € per year")
dev.off()

###############
###############

# scontandoli al 3% per 25 annni
library(FinCal)
benefit_npv <- npv(0.03, rep(150, 25))

benefit_npv/1e3  # NPV

# calcolando il discounted average yearly cost

eaa_from_npv <- function(npv, r, T) {
  npv * r / (1 - (1 + r)^(-T))
}

benefit_eea <- eaa_from_npv(benefit_npv, 0.03, 25)

benefit_eea/1e3  # EEA

###

sum(benefit_npv) / sum(rr_npv)
benefit_eea / sum(rr_eea)

###

library(xtable)

ms <- as.data.frame(na.omit(results %>% filter(Desc.Titolo.Spese.Arconet=="Spese in conto capitale") %>% dplyr::select(GVI_mean, Tot..Pag._mill, Esercizio.Finanziario) %>% ungroup()))

ms2 <- as.data.frame(na.omit(results %>% filter(Desc.Titolo.Spese.Arconet=="Spese correnti") %>% dplyr::select(GVI_mean, Tot..Pag._mill, Esercizio.Finanziario) %>% ungroup()))

ms <- merge(ms, ms2, by=c("GVI_mean", "Esercizio.Finanziario"))

colnames(ms)[3:4] <- c("Investment", "Expenditure")

ms$Investment <- ms$Investment * 1e6
ms$Expenditure <- ms$Expenditure * 1e6

sum_tab <- data.frame(
  Min = apply(ms, 2, min),
  Q1  = apply(ms, 2, quantile, probs = 0.25),
  Median = apply(ms, 2, median),
  Mean = apply(ms, 2, mean),
  Q3  = apply(ms, 2, quantile, probs = 0.75),
  Max = apply(ms, 2, max),
  SD  = apply(ms, 2, sd)
)

print(xtable(sum_tab, digits=3), file="tab_openbdap.tex")

##########
##########


result <- read_rds("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/bilanci_italia/budgets_comuni_Tutela_valorizzazione_recupero_ambientale_2017_2023_poparea.rds") %>% st_set_geometry(NULL)

result <- filter(result, Desc.Titolo.Spese.Arconet %in% c("Spese in conto capitale"))
result <- filter(result, Descrizione.Tipologia.Soggetto=="COMUNI")

result <- result %>% dplyr::group_by(COMUNE.y) %>% dplyr::summarise(Tot..Pag.=mean(Tot..Pag., na.rm=T))

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp") %>% dplyr::select(COMUNE)

result <- merge(result, comuni, by.x="COMUNE.y", by.y="COMUNE")


####

ggplot(st_as_sf(result))+
  geom_sf(aes(fill=Tot..Pag.+1), lwd=0, colour="transparent")+
  theme_void()+
  scale_fill_distiller(palette = "YlOrRd", trans="log",
                       breaks = c(100, 1000, 1e5, 1e6),
                       guide = guide_colorbar(
                         barwidth = 10,   # make legend bar wider
                         barheight =1  # keep it slim vertically
                       ))+
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggsave("fig_openbdap.png", width=4, height = 6)

ggplot(st_as_sf(result))+
  theme_classic()+
  geom_histogram(aes(x=log(Tot..Pag.+1)))

ggsave("fig_openbdap_hist.png", width=6, height = 6)
