
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(raster)
library(exactextractr)
library(sf)

# Set users
user <- 'gf'
#user <- 'edc'

if (user=='gf') {
  stub <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data'
}

if (user=='edc') {
  stub <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data'
}

setwd(stub)

###

data <- read_dta("month_2g_2020_2023.dta")

data_p <- read_dta("month_2g_2020_2023_withprices.dta")

###

data$CAP <- ifelse(nchar(data$CAP)==2, paste0("000", data$CAP), data$CAP)

View(data %>% group_by(CAP) %>% summarise(n=n()))

length(unique(data$pod))
table(data$year)
table(data$month)
length(unique(data$year))*length(unique(data$month))

# pblapply(unique(data$pod)[1:10], function(X) sum(grepl(X, data$pod)))

#############

### extract climate data (era 5)

era5_tmax_daily_comuni_2011_2023 <- read_rds("daily_tas_tmax_tmin_comune_2016_2023.rds")

era5_tmax_daily_comuni_2011_2023_s <- group_by(era5_tmax_daily_comuni_2011_2023, ISO3, year, month) %>% dplyr::summarise(tmax=mean(tmax, na.rm=T), tas=mean(tas, na.rm=T), tmin=mean(tmin, na.rm=T))

era5_tmax_daily_comuni_2011_2023_s_bins <- era5_tmax_daily_comuni_2011_2023

era5_tmax_daily_comuni_2011_2023_s_bins <- era5_tmax_daily_comuni_2011_2023_s_bins %>%
  mutate(
    Q_botom_tmax = case_when(tmax <= quantile(tmax, 0.05) ~ 1, TRUE ~ 0), 
    Q1_tmax = case_when(tmax > quantile(tmax, 0.05) & tmax <= quantile(tmax, 0.2) ~ 1, TRUE ~ 0),
    Q2_tmax = case_when(tmax > quantile(tmax, 0.2) & tmax <= quantile(tmax, 0.4) ~ 1, TRUE ~ 0),
    Q3_tmax = case_when(tmax > quantile(tmax, 0.4) & tmax <= quantile(tmax, 0.6) ~ 1, TRUE ~ 0),
    Q4_tmax = case_when(tmax > quantile(tmax, 0.6) & tmax <= quantile(tmax, 0.8) ~ 1, TRUE ~ 0),
    Q5_tmax = case_when(tmax > quantile(tmax, 0.8) & tmax <= quantile(tmax, 0.95) ~ 1, TRUE ~ 0),
    Q_top_tmax = case_when(tmax > quantile(tmax, 0.95) ~ 1, TRUE ~ 0))

era5_tmax_daily_comuni_2011_2023_s_bins <- group_by(era5_tmax_daily_comuni_2011_2023_s_bins, ISO3, year, month) %>% dplyr::summarise_at(colnames(era5_tmax_daily_comuni_2011_2023_s_bins)[9:15], sum, na.rm=T)

##

era5_tas_daily_comuni_2011_2023_s_bins <- era5_tmax_daily_comuni_2011_2023

era5_tas_daily_comuni_2011_2023_s_bins <- era5_tas_daily_comuni_2011_2023_s_bins %>%
  mutate(
    Q_botom_tas = case_when(tas <= quantile(tas, 0.05) ~ 1, TRUE ~ 0), 
    Q1_tas = case_when(tas > quantile(tas, 0.05) & tas <= quantile(tas, 0.2) ~ 1, TRUE ~ 0),
    Q2_tas = case_when(tas > quantile(tas, 0.2) & tas <= quantile(tas, 0.4) ~ 1, TRUE ~ 0),
    Q3_tas = case_when(tas > quantile(tas, 0.4) & tas <= quantile(tas, 0.6) ~ 1, TRUE ~ 0),
    Q4_tas = case_when(tas > quantile(tas, 0.6) & tas <= quantile(tas, 0.8) ~ 1, TRUE ~ 0),
    Q5_tas = case_when(tas > quantile(tas, 0.8) & tas <= quantile(tas, 0.95) ~ 1, TRUE ~ 0),
    Q_top_tas = case_when(tas > quantile(tas, 0.95) ~ 1, TRUE ~ 0))

era5_tas_daily_comuni_2011_2023_s_bins <- group_by(era5_tas_daily_comuni_2011_2023_s_bins, ISO3, year, month) %>% dplyr::summarise_at(colnames(era5_tas_daily_comuni_2011_2023_s_bins)[9:15], sum, na.rm=T)

##

era5_tas_daily_comuni_2011_2023_s_cddhdd <- era5_tmax_daily_comuni_2011_2023

era5_tas_daily_comuni_2011_2023_s_cddhdd <- era5_tas_daily_comuni_2011_2023_s_cddhdd %>% group_by(ISO3, year, month) %>% dplyr::summarise(CDD=sum(ifelse(tas>24, tas-24, 0), na.rm = T), HDD=sum(ifelse(tas<18, 18-tas, 0), na.rm = T))

######

mm <- merge(data, era5_tmax_daily_comuni_2011_2023_s, by.x=c("comune", "year", "month"), by.y=c("ISO3", "year", "month"))
mm <- merge(mm, era5_tas_daily_comuni_2011_2023_s_bins, by.x=c("comune", "year", "month"), by.y=c("ISO3", "year", "month"))
mm <- merge(mm, era5_tmax_daily_comuni_2011_2023_s_bins, by.x=c("comune", "year", "month"), by.y=c("ISO3", "year", "month"))
mm <- merge(mm, era5_tas_daily_comuni_2011_2023_s_cddhdd, by.x=c("comune", "year", "month"), by.y=c("ISO3", "year", "month"))
# extract UGS (CAP/comune level?)

library(sf)

comuni <- read_sf("Com01012023_g_WGS84.shp")
comuni <- comuni %>% dplyr::select(COD_PROV, COMUNE)
comuni <- filter(comuni, COMUNE %in% mm$comune)

#############

GVI_stats_comuni_2016_2023<-readRDS("GVI_stats_comuni_2016_2023.RDS")
colnames(GVI_stats_comuni_2016_2023)

gvi_mean<-GVI_stats_comuni_2016_2023[,c(8,12,29,33,37,41)]
colnames(gvi_mean)
gvi_mean<-gvi_mean%>%gather("year","gvi_mean",GVI_mean_2020:GVI_mean_2023)
gvi_mean$year<-substr(gvi_mean$year,10,13)

rm(GVI_stats_comuni_2016_2023); gc()
gvi_mean_bk <- gvi_mean
gvi_mean$geometry <- NULL

df <- merge(mm, gvi_mean, by.x=c("comune", "year"), by.y=c("COMUNE", "year"))
df$year <- as.numeric(df$year)


### some simple panel regressions

df$month <- as.factor(df$month)
df$year <- as.factor(df$year)

df <- filter(df, cons_month>0)
df$hotmonths <- as.factor(ifelse(as.numeric(df$month)>5 & as.numeric(df$month)<10, 1, 0))

###


r <- read_rds("F:/.shortcut-targets-by-id/1JhN0qxmpnYQDoWQdBhnYKzbRCVGH_WXE/6-Projections/data/household/Italy/italy_hbs.rds")

r <- r %>% group_by(adm1) %>% dplyr::summarise(ac=weighted.mean(as.numeric(as.character(ac)), weight, na.rm=T))

df <- merge(df, r, by.x="region", by.y = "adm1")

###

pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")

comuni$pop <- exact_extract(pop, comuni, "sum")

comuni <- dplyr::select(comuni, COMUNE, pop)

comuni$area <- as.numeric(st_area(comuni))/1e6

comuni$popdens <- comuni$pop / comuni$area

comuni$geometry <- NULL

df <- merge(df, comuni, by.x="comune", by.y="COMUNE", all.x=T)

df$type <- ifelse(df$popdens > 500, "urban", ifelse(df$popdens<500 & df$popdens>250, "semiurban", "rural"))

df$type <- ifelse(is.na(df$popdens), NA, df$type)

#########

library(tidyverse)

r <- "F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/MEF_REDDITIIRPEF_COM_22052024202142385.csv"

r <- read.csv(r)

r1 <- r[r$Tipo.dato=="reddito imponibile (euro)",]
r2 <- r[r$Tipo.dato=="contribuenti con reddito imponibile",]

r2 <- dplyr::select(r2, Territorio, TIME, Value)

r1 <- merge(r1, r2, c("Territorio", "TIME"))

r1$Value <- r1$Value.x/r1$Value.y

r1 <- dplyr::select(r1, Territorio, TIME, Value)

##

df$year <- as.numeric(as.character(df$year))

df <- merge(df, r1, by.x=c("comune", "year"), by.y=c("Territorio", "TIME"), all.x=T)

df <- df %>% group_by(comune) %>% dplyr::mutate(reddito_medio=mean(Value, na.rm=T), Value=NULL)

saveRDS(df, "processed_data.rds")

####

# collapse data at the comune level 

df_c <- df %>% filter(cons_month>50) %>% group_by(comune, region, year, month) %>% dplyr::summarise(cons_month=mean(cons_month, na.rm=T), cons_719_month=mean(cons_719_month, na.rm=T), pop = mean(pop, na.rm=T),  popdens = mean(popdens, na.rm=T),  tmax = mean(tmax, na.rm=T),  gvi_mean = mean(gvi_mean, na.rm=T), reddito_medio=mean(reddito_medio, na.rm=T))

saveRDS(df_c, "processed_data_comuni.rds")
