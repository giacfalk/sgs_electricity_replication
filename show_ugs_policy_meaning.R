
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)
library(raster)
library(exactextractr)
library(sf)
library(terra)

stub <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub)

out_ndvi_m <- read_rds("data_provide_cdh_gvi_143cities.rds")

#

out_ndvi_m <- filter(out_ndvi_m, city=="Rome")

out_ndvi_m <- st_as_sf(out_ndvi_m, coords = c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(250) %>% st_transform(4326)

#

pop <- rast("hrsl_pop/ita_general_2020.tif")

out_ndvi_m$pop <- exact_extract(pop, out_ndvi_m, "mean")

###

out_ndvi_m <- filter(out_ndvi_m, pop>summary( out_ndvi_m$pop)[5] &  (pop < (summary(out_ndvi_m$pop)[5] + 0.1*(summary(out_ndvi_m$pop)[5]))))

# Calculate the 1st quantile
quantile_value <- quantile(out_ndvi_m$out_b_mean, probs = 0.2)

# Find observations at the 1st quantile boundary
observations_at_quantile <- out_ndvi_m[which.min(abs(out_ndvi_m$out_b_mean - quantile_value)), ]

library(googleway)

key <- "AIzaSyABAy4zgNg6P41_RlFIEa0JBo9W8o4866g"

stub <- 'F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data'
setwd(stub)

google_streetview(location = c(observations_at_quantile$y, observations_at_quantile$x),
                  size = c(1200,1200),
                  output = "plot",
                  heading = 220,
                  fov = 120,
                  pitch = 0,
                  response_check = FALSE,
                  key = key)


dev.print(pdf, paste0('rome_q1_gvi_', round(observations_at_quantile$out_b_mean, 0), '.pdf'))

###

# Calculate the 1st quantile
quantile_value <- quantile(out_ndvi_m$out_b_mean, probs = 0.9)

# Find observations at the 1st quantile boundary
observations_at_quantile <- out_ndvi_m[which.min(abs(out_ndvi_m$out_b_mean - quantile_value)), ]

library(googleway)

key <- "AIzaSyABAy4zgNg6P41_RlFIEa0JBo9W8o4866g"

google_streetview(location = c(observations_at_quantile$y, observations_at_quantile$x),
                  size = c(1200,1200),
                  output = "plot",
                  heading = 270,
                  fov = 120,
                  pitch = 0,
                  response_check = FALSE,
                  key = key)



dev.print(pdf, paste0('rome_q3_gvi_', round(observations_at_quantile$out_b_mean, 0), '.pdf'))

#####
