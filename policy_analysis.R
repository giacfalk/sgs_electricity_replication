
# sgs expansion and climate change simulations

# https://nex-gddp-cmip6.s3.us-west-2.amazonaws.com/index.html#NEX-GDDP-CMIP6/CMCC-CM2-SR5/ssp585/r1i1p1f1/tasmax/

library(sf)
library(raster)
library(reshape2)
library(terra)

comuni <- read_sf("Com01012023_g_WGS84.shp")
comuni <- comuni %>% dplyr::select(COD_PROV, COMUNE)

#

quantile(df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean), 0.75)

summary(df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))


#

closest_percentile <- function(value, data) {
  data <- sort(data)
  percentiles <- ecdf(data)(data) * 100
  value_percentile <- ecdf(data)(value) * 100
  idx <- which.min(abs(percentiles - value_percentile))
  return(value_percentile)
}


closest_percentile(15, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(18, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(20, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(21, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(24, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(25, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))
closest_percentile(27, df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))

#

# calculate treatment shares


length((15 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))[(15 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))>0]) / length((15 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean)))

length((20 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))[(20 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))>0]) / length((20 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean)))

length((21 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))[(21 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))>0]) / length((21 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean)))

length((24 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))[(24 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))>0]) / length((24 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean)))

length((27 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))[(27 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))>0]) / length((27 - df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean)))


#

DT <- read_rds("tasmax_daily_2041-2060_minus_1995-2014_CMCC-ESM2.RDS")

s <- rast(pblapply(1:366, function(X){DT_doy1 <- DT[doy == X]; return(rotate(rast(DT_doy1[, .(x, y, tmx_his)]))) }))
s2 <- rast(pblapply(1:366, function(X){DT_doy1 <- DT[doy == X]; return(rotate(rast(DT_doy1[, .(x, y, tmx_585)]))) }))

dates <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day")

###

comuni <- st_transform(comuni, 4326)

library(exactextractr)
library(lubridate)

names(s) <- names(s2) <- dates

s <- exact_extract(s, comuni, "mean")
s$comune <- comuni$COMUNE
s <- reshape2::melt(s, ncol(s))
s$variable <- gsub("mean.", "", s$variable)
s$date <- as.Date(s$variable, "%Y-%m-%d")
s$month <- month(s$date)
s$year <- year(s$date)

s <- group_by(s, comune, year, month) %>% dplyr::summarise(tmax=mean(value, na.rm=T))

s2 <- exact_extract(s2, comuni, "mean")
s2$comune <- comuni$COMUNE
s2 <- reshape2::melt(s2, ncol(s2))
s2$variable <- gsub("mean.", "", s2$variable)
s2$date <- as.Date(s2$variable, "%Y-%m-%d")
s2$month <- month(s2$date)
s2$year <- year(s2$date)

s2 <- group_by(s2, comune, year, month) %>% dplyr::summarise(tmax=mean(value, na.rm=T))

###

s$delta_tmax_2050_ssp585 <- s2$tmax - s$tmax
s$tmax <- NULL
s$year <- NULL

###

df_merged_policy <- merge(df_comuni, s, by=c("comune", "month"))

###

# 1: projected increase in ELY in response to temperature, grouped by UGS class

df_merged_policy$tmax_2050_ssp585 <- df_merged_policy$tmax  + df_merged_policy$delta_tmax_2050_ssp585

# 2: policy to increase UGS: what benefit for reducing ely?

sum_tab <- data.frame("avg. % reduction", "TWh/summer", "% yearly resid. electr. cons.", "MT CO2", "Ely. tot (TWh/summer)")
colnames(sum_tab) <- c("avg. % reduction", "TWh/summer", "% yearly resid. electr. cons.", "MT CO2", "Ely. tot (TWh/summer)")

scens <- expand.grid(gvi=c(0, 15, 20, 21, 24, 27), tmax=c("tmax", "tmax_2050_ssp585"))

df_merged_policy$tmax_bk <- df_merged_policy$tmax

ll <- list()

ll_expenditure <- list()

###

for(scen in 1:nrow(scens)){
  
  df_policy <- df_merged_policy
  
  target <- scens$gvi[scen]
  df_policy$tmax <- df_policy[,as.character(scens$tmax[scen])]
  df_policy$gvi_mean_orig <- df_policy$gvi_mean
  
  ###
  
  # target based policy
  df_policy$gvi_mean <- ifelse(df_policy$gvi_mean_orig<target, target, df_policy$gvi_mean_orig)
  
  # percentage based policy
  # df_policy$gvi_mean <- df_policy$gvi_mean_orig*1.1
  
  ###
  
  pr1 <- exp(predict(reg_quadratic, newdata = df_merged_policy))
  pr2 <- exp(predict(reg_quadratic, newdata = df_policy))
  
  pr1 <- pr1[as.numeric(df_merged_policy$month)>5 & as.numeric(df_merged_policy$month)<9]
  pr2 <- pr2[as.numeric(df_merged_policy$month)>5 & as.numeric(df_merged_policy$month)<9]
  
  pr_diff <- 1- (pr2 / pr1)
  
  df_policy <- df_policy[as.numeric(df_merged_policy$month)>5 & as.numeric(df_merged_policy$month)<9,]
  
  df_policy$pr <- pr2
  
  df_policy$delta <- pr_diff
  
  ##
  
  df_policy_s <- dplyr::group_by(df_policy, comune, month, region, year) %>% dplyr::summarise(avg_cons_719_month=mean(cons_719_month, na.rm=T), avg_delta_frac=mean(delta, na.rm=T), avg_delta_abs=mean(delta*cons_719_month, na.rm=T)) %>% mutate(scenario=scen)
  
  ll_expenditure[[scen]] <- df_policy_s
  
  ##
  
  library(geodata)
  library(sf)
  
  province <- st_as_sf(gadm(country = "ITA", level = 3, path = getwd()))
  province$comune <- province$NAME_3
  
  df_policy <- df_policy %>% group_by(comune) %>% dplyr::summarise(delta=mean(delta, na.rm=T), gvi_mean=mean(gvi_mean, na.rm=T), gvi_mean_orig=mean(gvi_mean_orig, na.rm=T), cons_719_month=mean(cons_719_month, na.rm=T), pop=mean(pop, na.rm=T), pr=mean(pr, na.rm=T)) %>% ungroup() %>% group_by(comune) %>%  dplyr::summarise(delta=mean(delta, na.rm=T), gvi_mean=mean(gvi_mean, na.rm=T), gvi_mean_orig=mean(gvi_mean_orig, na.rm=T), cons_719_month=mean(cons_719_month, na.rm=T), pop=mean(pop, na.rm=T), pr=mean(pr, na.rm=T))
  
  library(fuzzyjoin)
  
  df_policyy <- stringdist_left_join(df_policy,province, by="comune", distance_col = "distance")
  
  df_policyy <- df_policyy %>% group_by(comune.x) %>% filter(distance==min(distance))
  
  ##############
  
  nhouseholds <- 26e6
  
  df_policy_sf <- st_as_sf(df_policyy)
  
  a <- ggplot()+
    theme_void()+
    geom_sf(data=df_policy_sf, aes(fill=-delta*100), colour="transparent")+
    geom_sf(data=province, fill="transparent", colour="black", lwd=0.01)+
    scale_fill_distiller(name="", palette = "YlOrRd", direction = 1)
  
  ll[[scen]] <- a
  # 
  # library(patchwork)
  # 
  # ggsave(paste0("policy_", target, ".png"), a, width=4.5, heigh=3, scale=2, bg = "white")
  # 
  png(paste0("hist_policy_", target, ".png"), height = 600, width = 600)
  hist(df_policyy$delta*100, main="Expected Summer monthly consumption reduction if UGS >=35 policy implemented", xlab="")
  dev.off()
  
  ##############
  i1 <- weighted.mean(df_policyy$delta*100, df_policyy$cons_719_month, na.rm=T)
  
  i2 <- weighted.mean(df_policyy$delta * df_policyy$cons_719_month * 3, df_policyy$pop, na.rm = T) * nhouseholds / 1e9 # twh
  
  i3 <- (weighted.mean(df_policyy$delta * df_policyy$cons_719_month * 3, df_policyy$pop, na.rm = T) * nhouseholds  * 372) / 1e12 # mtco2
  
  i4 <- (((weighted.mean(df_policyy$delta * df_policyy$cons_719_month * 3, df_policyy$pop, na.rm = T) * nhouseholds)  / 1e9) / 64.5) * 100  #% ot total ely
  # https://www.terna.it/it/sistema-elettrico/statistiche/evoluzione-mercato-elettrico/consumi-totale
  
  i5 <- weighted.mean(df_policyy$pr * 3, df_policyy$pop, na.rm = T) * nhouseholds / 1e9
  
  print(i5)
  
  c(i1, i2, i4, i3)
  
  sum_tab[match(scen, 1:nrow(scens)),] <- c(i1, i2, i4, i3, i5)
  
}

############
###########

write_rds(ll_expenditure, "scenarios_outputs_comunelevel.rds")

sum_tab <- sum_tab %>% mutate_all(as.numeric) %>% mutate_if(is.numeric, round, 2)
rownames(sum_tab) <- paste0(scens$gvi, "_", scens$tmax)

sum_tab[,1] <- - sum_tab[,1]
sum_tab[,2] <- - sum_tab[,2]
sum_tab[,3] <- - sum_tab[,3]
sum_tab[,4] <- - sum_tab[,4]

colnames(sum_tab)[1] <- "avg. % change"

stargazer::stargazer(sum_tab[-1,], digits=2, digits.extra=2, summary = F, out = "sim_res.tex")


##

library(patchwork)

a <- (ll[[7]] + ggtitle("SSP5(85) climate, no GVI policy") +scale_fill_distiller(name="", limits=c(0, 10), palette = "YlOrRd", direction = 1, na.value = "transparent")) +  (ll[[10]] + ggtitle("SSP5(85) climate, GVI>21 policy") +  scale_fill_distiller(name="", limits=c(0, 10), palette = "YlOrRd", direction = 1, na.value = "transparent")) + plot_layout(guides = "collect")

ggsave(paste0("policy_21_hist_585.png"), a, width=4.5, heigh=3, scale=2, bg = "white")


#########

19.87 - 18.50
18.89-18.5
0.39/1,37
