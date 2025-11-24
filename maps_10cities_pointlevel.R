
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

#################

comuni <- read_sf("F:/.shortcut-targets-by-id/1Pfp3HJp1Mi4OAdRT5vjYVfKF3lsLc5xK/arpav_econ/ARPAV Climate Data/shapefiles/comuni/Com01012023_g_WGS84.shp")

selected_comuni <- comuni %>% filter(COMUNE %in% c("Bari", "Bologna", "Genova", "Milano", "Napoli", "Padova", "Palermo", "Roma", "Trieste", "Torino")) %>% st_transform(4326) %>% st_centroid(.)

#

out_ndvi_m<-read_csv("C:/Users/falchetta/OneDrive - IIASA/Current papers/greening/output_dataset/gvi_358cities_2016_2023_yearly_falchetta_hammad.csv")

###

out_ndvi_m <- out_ndvi_m %>% filter(country=="IT")
out_ndvi_m <- dplyr::group_by(out_ndvi_m, city, x, y) %>% dplyr::summarise(GVI_predicted=mean(GVI_predicted, na.rm=T))

b <- pblapply(unique(out_ndvi_m$city), function(ctry){ ggplot(out_ndvi_m %>% filter(city==ctry))+
    theme_void()+
    geom_point(aes(x=x, y=y, colour=GVI_predicted), size=0.35)+
    scale_colour_distiller(name="GVI", palette = "YlGn", direction = 1, limits=c(5, 40))+
    ggtitle(ctry)})

library(patchwork)

plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}


b <- plot_a_list(b, 2, 5) + plot_layout(guides = "collect") & theme(legend.direction = "horizontal", legend.position = "bottom")

ggsave("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/gvi_ita_comuni.png", b, height = 2, width = 3.5, scale=3, bg="white")

###

out_ndvi_m <- st_as_sf(out_ndvi_m, coords=c("x", "y"), crs=4326, remove = F) %>% st_transform(3395) %>% st_buffer(50) %>% st_transform(4326)

stub2 <- "C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/"
setwd(stub2)

city_d=tolower(read.csv("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/provide_urban_climate_data/cities_provide.csv")[-91,1]) %>% gsub(" ", "_", .)

city_d <- city_d[grep(paste0(c("Bari", "Bologna", "Genoa", "Milan", "Naples", "Padua", "Palermo", "Rome", "Trieste", "Turin"), collapse="|"), city_d, ignore.case = T)]

v <- list.files(pattern = "cooling-degree-hours")
v <- v[grepl("absolute", v)]
v <- v[grepl("2020", v)]
v <- v[grepl("curpol", v)]
v <- v[grepl(paste(city_d, collapse="|"), v)]
v <- vrt(v, "my_10comuni.vrt", overwrite=T)
r <- rast("my_10comuni.vrt")

# 

library(rgis)

comuni = read_sf("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/boundaries/Com01012023_g_WGS84.shp") %>% st_transform(4326)
comuni <- dplyr::select(comuni, PRO_COM, COMUNE)


c <- pblapply(c("Bari", "Bologna", "Genova", "Milano", "Napoli", "Padova", "Palermo", "Roma", "Trieste", "Torino"), function(ctry){
  
  r_m <- mask(r, comuni %>% filter(COMUNE==ctry))
  r_m <- mask_raster_to_polygon(raster(r_m), comuni %>% filter(COMUNE==ctry))
  
  r_m <- as.data.frame(r_m, xy=T)
  r_m <- na.omit(r_m)
  r_m <- r_m[r_m$my_10comuni>0,]
  
  ggplot(r_m)+
    theme_void()+
    geom_point(aes(x=x, y=y, colour=my_10comuni), size=0.35)+
    scale_colour_distiller(name="CDH", palette = "YlOrRd", direction = 1, limits=c(0, 9000))+
    ggtitle(ctry)})


library(patchwork)

c <- plot_a_list(c, 2, 5) + plot_layout(guides = "collect") & theme(legend.direction = "horizontal", legend.position = "bottom")

ggsave("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/cdh_ita_comuni.png", c, height = 2, width = 3.5, scale=3, bg="white")
