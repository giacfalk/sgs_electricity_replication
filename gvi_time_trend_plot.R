
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

########

GVI_stats_comuni_2016_2023<-readRDS("GVI_stats_comuni_2016_2023.RDS")
colnames(GVI_stats_comuni_2016_2023)

gvi_mean<-GVI_stats_comuni_2016_2023[,c(7,12,17, 21, 25, 29,33,37,41)]
colnames(gvi_mean)
gvi_mean<-gvi_mean%>%gather("year","gvi_mean",GVI_mean_2017:GVI_mean_2023)
gvi_mean$year<-substr(gvi_mean$year,10,13)

rm(GVI_stats_comuni_2016_2023); gc()
gvi_mean_bk <- gvi_mean
gvi_mean$geometry <- NULL

library(patchwork)

a <- ggplot(gvi_mean, aes(x=year, y=gvi_mean))+
  geom_boxplot()+
  geom_smooth()

b <- ggplot(gvi_mean)+
  geom_boxplot(aes(x=year, y=gvi_mean))+
  facet_wrap(vars(COD_REG))

a + b + plot_layout(ncol=1)

ggsave("gvi_evol.png", width = 5, height = 7, scale=1.5)
