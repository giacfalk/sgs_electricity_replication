

rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
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

####

df <- read_rds("processed_data.rds")

df <- df[df$cons_719_month>50 & df$cons_719_month<15000,]

cdh <- read_rds("cdh_2020_2022_comuni.rds")

df <- merge(df, cdh, by=c("comune", "year", "month"))

####

library(geodata)

ita <- gadm(country = "ITA", level = 0, path = getwd())
ita <- st_as_sf(ita)

####

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

selected_comuni <- comuni %>% filter(COMUNE %in% c("Bari", "Bologna", "Genova", "Milano", "Napoli", "Padova", "Palermo", "Roma", "Trieste", "Torino")) %>% st_transform(4326) %>% st_centroid(.)

df_s <- df %>% group_by(comune, month) %>% dplyr::summarise(tmax=mean(tmax, na.rm=T), gvi_mean=mean(gvi_mean, na.rm=T), cons_719_month=median(cons_719_month, na.rm=T))

df_ss <- df %>% group_by(pod, comune, year) %>% dplyr::summarise(cons_719_month=sum(cons_719_month, na.rm=T)) %>% ungroup() %>% dplyr::group_by(comune) %>%  dplyr::summarise(cons_719_month=median(cons_719_month, na.rm=T))

formap <- merge(df_s, comuni, by.x="comune", by.y="COMUNE")
formap <- st_as_sf(formap)

###

library(ggthemes)

ggplot()+
  theme_void()+
  geom_sf(data=comuni, fill="transparent", colour="lightgrey")+
  geom_sf(data=formap, aes(fill=tmax), colour="transparent")+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  facet_wrap(vars(month))

ggsave("fig_comuni_tmax_bymonth.png", height=5, width = 5, scale=1.5, bg="white")

###

df_ss <- df %>% mutate(month=as.numeric(as.character(month))) %>%  filter(month>5 & month<9) %>%  dplyr::group_by(comune) %>%  dplyr::summarise(tmax=mean(tmax, na.rm=T), cdh=mean(cdh, na.rm=T))

formap_3 <- merge(df_ss, comuni, by.x="comune", by.y="COMUNE")
formap_3 <- st_as_sf(formap_3)

a_p <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=formap_3, aes(fill=tmax), colour="transparent")+
  geom_sf_text(data=selected_comuni, aes(label=COMUNE))+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name="°C", limits=c(15, 35))+
  ggtitle("Mean monthly maximum temperature, Summer months, 2020-2022")

b_p <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=formap_3, aes(fill=cdh), colour="transparent")+
  geom_sf_text(data=selected_comuni, aes(label=COMUNE))+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name="#")+
  ggtitle("Mean monthly Cooling Degree Hours, Summer months, 2020-2022")

library(patchwork)

ggsave("fig_comuni_tmax_cdh_avg.png", a_p + b_p, height=5, width = 9, scale=1.5, bg="white")

###

df$hotmonths <- ifelse(as.numeric(df$month)>5 & as.numeric(df$month)<9, 1, 0)

df_ss <- df %>% group_by(pod, comune, year) %>% dplyr::summarise(cons_719_month=mean(cons_719_month, na.rm=T)) %>% ungroup() %>% dplyr::group_by(comune) %>%  dplyr::summarise(cons_719_month=median(cons_719_month, na.rm=T))
breakss = quantile(df_ss$cons_719_month, seq(0, 1, 0.2))
df_ss$cons_719_month_f <- cut(df_ss$cons_719_month, breakss)

formap_3 <- merge(df_ss, comuni, by.x="comune", by.y="COMUNE")
formap_3 <- st_as_sf(formap_3)

summary(df_ss$cons_719_month)

a_p <- ggplot()+
  theme_void()+
  geom_sf(data=comuni, fill="transparent", colour="lightgrey")+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=formap_3, aes(fill=cons_719_month_f), colour="transparent")+
  scale_fill_brewer(palette = "YlOrRd", direction = 1, name="kWh per year", na.translate=F)+
  ggtitle("Average monthly electricity consumption per POD, by municipality")

ggsave("fig_comuni_cons_avg.png", a_p, height=5, width = 5, scale=1.5, bg="white")

###

df_ss <- df %>% filter(hotmonths==1) %>% group_by(pod, comune, year) %>% dplyr::summarise(cons_719_month=mean(cons_719_month, na.rm=T)) %>% ungroup() %>% dplyr::group_by(comune) %>%  dplyr::summarise(cons_719_month=median(cons_719_month, na.rm=T))
df_ss$cons_719_month_f <- cut(df_ss$cons_719_month, breakss)

formap_3 <- merge(df_ss, comuni, by.x="comune", by.y="COMUNE")
formap_3 <- st_as_sf(formap_3)

summary(df_ss$cons_719_month)

b_p <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=formap_3, aes(fill=cons_719_month_f), colour="transparent")+
  scale_fill_brewer(palette = "YlOrRd", direction = 1, name="kWh/month", na.translate=F)+
  ggtitle("Average summer-month electricity consumption per POD, by municipality")

ggsave("fig_comuni_cons_summer.png", b_p, height=5, width = 5, scale=1.5, bg="white")

library(patchwork)

a_p + theme(legend.position = "none") + b_p

ggsave("fig_comuni_cons_avg_both.png", height=5, width = 9, scale=1.5, bg="white")

#

df_ss <- df %>% group_by(pod, comune, year) %>% dplyr::summarise(cons_share=sum(cons_719_month[hotmonths==1], na.rm=T)/sum(cons_719_month, na.rm=T)) %>% ungroup() %>% dplyr::group_by(comune) %>%  dplyr::summarise(cons_share=median(cons_share, na.rm=T))

formap_3 <- merge(df_ss, comuni, by.x="comune", by.y="COMUNE")
formap_3 <- st_as_sf(formap_3)

summary(df_ss$cons_share)

formap_3$cons_share_f <- cut(formap_3$cons_share, c(0, 0.1, 0.175, 0.25, 0.5, 1))

c_p <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=formap_3, aes(fill=cons_share_f), colour="transparent")+
  scale_fill_brewer(palette = "YlOrRd", direction = 1, name="%", na.translate=F)

c_p_h <- ggplot()+
  theme_classic()+
  geom_histogram(data=formap_3 %>% filter(cons_share>0 & cons_share<1), aes(x=cons_share*100))+
  xlab("% summer-month consumption")

ggsave("fig_comuni_cons_share.png", c_p_h, height=5, width = 6, scale=.75, bg="white")


###

formap_2 <- dplyr::filter(formap, month==1)

library(ggthemes)

ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=comuni, fill="transparent", colour="lightgrey")+
  geom_sf(data=comuni, fill="transparent", colour="transparent")+
  geom_sf(data=formap_2, aes(fill=gvi_mean), colour="transparent")+
  scale_fill_distiller(palette = "Greens", direction = 1, name="GVI")+
  geom_sf_text(data=selected_comuni, aes(label=COMUNE))+

ggsave("fig_provinces_gvi.png", height=5, width = 5, scale=1.5, bg="white")

###

formap_2 <- formap

library(ggthemes)

a <- ggplot()+
  theme_void()+
  geom_sf(data=comuni, fill="transparent", colour="lightgrey")+
geom_sf(data=formap_2 %>% filter(cons_719_month<200), aes(fill=cons_719_month), colour="transparent")+
  scale_fill_distiller(name="avg kWh/hh./month", palette = "YlOrRd", direction = 1)+
  facet_wrap(vars(month))

ggsave("fig_provinces_cons.png", a, height=5, width = 5, scale=1.5, bg="white")


###


df <- read_rds("processed_data.rds")

df <- df[df$cons_719_month>50,]

df_s <- df %>% dplyr::group_by(pod, comune, year) %>% dplyr::summarise(cons_719_month=sum(cons_719_month, na.rm=T)) %>% ungroup() %>% group_by(comune) %>% dplyr::summarise(cons_719_month=mean(cons_719_month, na.rm=T))

####

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

formap <- merge(df_s, comuni, by.x="comune", by.y="COMUNE")
formap <- st_as_sf(formap)


ggplot()+
  theme_void()+
  geom_sf(data=comuni, fill="transparent", colour="lightgrey")+
  geom_sf(data=formap, aes(fill=cons_719_month), colour="transparent")+
  scale_fill_distiller(name="avg kWh/yr.", palette = "YlOrRd", direction = 1)

###

df$macroregion <- NA

df$macroregion[df$region %in% c("Veneto", "Piemonte", "Lombardia", "Emilia-Romagna", "Friuli-Venezia Giulia", "Liguria")] <- "Nord"
df$macroregion[df$region %in% c("Marche", "Lazio", "Abruzzo", "Umbria", "Toscana")] <- "Centro"
df$macroregion[df$region %in% c("Sicilia", "Sardegna", "Abruzzo", "Basilicata", "Campania", "Calabria", "Puglia", "Molise")] <- "Sud e isole"

dff <- df %>% group_by(pod, month, macroregion) %>% dplyr::summarise(cons_719_month=mean(cons_719_month, na.rm=T))

ggplot(data=dff)+
  theme_classic()+
  gg.layers::geom_boxplot2(aes(x=month, y=cons_719_month, fill=macroregion), width.errorbar = 0.1)+
  ylab("kWh/month, 2020-2022 average")

ggsave("boxplots_cons.png", height=3, width = 5, scale=1.4, bg="white")

######
######



rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
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

####

df <- read_rds("processed_data.rds")

df <- df[df$cons_719_month>50 & df$cons_719_month<15000,]

cdh <- read_rds("cdh_2020_2022_comuni.rds")

df <- merge(df, cdh, by=c("comune", "year", "month"))

####

library(geodata)

ita <- gadm(country = "ITA", level = 0, path = getwd())
ita <- st_as_sf(ita)

####

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

df_s <- df %>% filter(month==1)  %>% group_by(comune, year) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% ungroup() %>% arrange(comune, year) %>% dplyr::group_by(comune) %>% dplyr::summarise(gvi_mean_abs_delta = last(gvi_mean) - first(gvi_mean), gvi_mean_pct_delta = (last(gvi_mean) / first(gvi_mean)) - 1)

formap <- merge(df_s, comuni, by.x="comune", by.y="COMUNE")
formap <- st_as_sf(formap)

a1 <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=comuni, fill="transparent", colour="transparent")+
  geom_sf(data=formap, aes(fill=gvi_mean_abs_delta), colour="transparent")+
  scale_fill_gradient2(name="GVI")

a2 <- ggplot()+
  theme_void()+
  geom_sf(data=ita, fill="transparent", colour="black")+
  geom_sf(data=comuni, fill="transparent", colour="transparent")+
  geom_sf(data=formap, aes(fill=gvi_mean_pct_delta*100), colour="transparent")+
  scale_fill_gradient2(name="GVI")

###

ggsave("fig_provinces_gvi_change_abs.png", a1, height=5, width = 5, scale=1.5, bg="white")

ggsave("fig_provinces_gvi_change_pct.png", a2, height=5, width = 5, scale=1.5, bg="white")

