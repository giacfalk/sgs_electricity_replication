
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() 

# Set required packages
library(haven)
library(tidyverse)
library(pbapply)
library(fixest)
library(marginaleffects)

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

# import POD-level version of the data

df <- read_rds("processed_data.rds")
df <- df[df$cons_719_month>50,]

df$covid <- ifelse(df$year==2020, "1", "0")
df$quartile_reddito <- cut(df$reddito_medio, quantile(df$reddito_medio, by=seq(0, 1, 0.2), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"))

###

summary(df %>% group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T)) %>% pull(gvi_mean))

###

df_distr <- df
df_distr$tmax <- cut(df_distr$tmax, c(seq(10,35, by=5)))
df_distr$gvi_mean <- cut(df_distr$gvi_mean, c(c(10, 15, 20, 25, 30)))

df_freq <- df_distr %>% ungroup() %>% 
  dplyr::count(tmax, gvi_mean) %>%
  dplyr::mutate(freq = n / sum(n))

df_freq <- na.omit(df_freq)

ggplot(df_freq) +
  geom_tile(aes(x = tmax, y = gvi_mean, fill = freq*100)) +
  geom_text(aes(x = tmax, y = gvi_mean, label = n))+
  scale_fill_distiller(name="%", palette = "Reds", direction = 1)+
  xlab("Monthly average Tmax")+
  ylab("Street Green Space (GVI)")+
  ggtitle("Frequency of observations in the POD-level dataset")

ggsave("distribution_heatmap_719.png", height=4.5, width = 6, scale=.8, bg="white")

##########################################
##########################################
# proto specification 

reg_proto1 <- feols(log(cons_719_month) ~ tmax | comune + pod + year + month + region^year, data=df)
reg_proto2 <- feols(log(cons_719_month) ~ tmax + tmax^2 | comune + pod + year + month + region^year, data=df)

etable(reg_proto1, reg_proto2, cluster = ~comune+pod)

sink(file="reg_tab_proto_719.tex")
etable(reg_proto1, reg_proto2, cluster = ~comune+pod, tex=T, replace = T)
sink()

##########################################
##########################################
# main specification 

reg_def1 <- feols(log(cons_719_month) ~ tmax + gvi_mean + tmax:gvi_mean | pod, data=df)

reg_def2 <- feols(log(cons_719_month) ~ tmax + gvi_mean + tmax:gvi_mean  | comune + pod, data=df)

reg_def3 <- feols(log(cons_719_month) ~ tmax + gvi_mean + tmax:gvi_mean  | comune + pod + year, data=df)

reg_def4 <- feols(log(cons_719_month) ~ tmax + gvi_mean + tmax:gvi_mean | comune + pod + year + month, data=df)

reg_def5 <- feols(log(cons_719_month) ~ tmax + gvi_mean + tmax:gvi_mean  | comune + pod + year + month  , data=df, combine.quick = F)

reg_quadratic <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df, combine.quick = F)

etable(reg_def2, reg_def3, reg_def4, reg_def5, reg_quadratic, cluster = ~comune+pod)

sink(file="reg_tab_719.tex")
etable(reg_def2, reg_def3, reg_def4, reg_def5, reg_quadratic, cluster = ~comune+pod, tex=T, replace = T)
sink()

###

ame_gvi_2 <- avg_slopes(
  reg_def2,
  variables = "gvi_mean",
  vcov = F)


ame_gvi_3 <- avg_slopes(
  reg_def3,
  variables = "gvi_mean",
  vcov = F)


ame_gvi_4 <- avg_slopes(
  reg_def4,
  variables = "gvi_mean",
  vcov = F)


ame_gvi_5 <- avg_slopes(
  reg_def5,
  variables = "gvi_mean",
  vcov = F)

ame_gvi_quadratic <- avg_slopes(
  reg_quadratic,
  variables = "gvi_mean",
  vcov = F)

round(na.omit(c(as.numeric(ame_gvi_2), as.numeric(ame_gvi_3), as.numeric(ame_gvi_4), as.numeric(ame_gvi_5), as.numeric(ame_gvi_quadratic))), 4)

###

# main curve figure

dd <- predictions(reg_quadratic, variables=list(tmax=seq(5, 35, 5), gvi_mean=c(10, 15, 20, 25, 30)), transform = exp, vcov = F)

summary(dd$estimate)

ddd <- dd %>% group_by(tmax, gvi_mean) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T))

ggplot(ddd %>% filter(gvi_mean!="30"))+
  geom_line(aes(x=tmax, y=estimate, colour=as.factor(gvi_mean), group=as.factor(gvi_mean)))+
  scale_colour_brewer(palette = "Greens", name="GVI")+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip_719.text.x = element_blank()
  )+
  xlab("Monthly average maximum temperature (°C)")+
  ylab("Predicted monthly electricity consumption (kWh)")

ggsave("lines_urgs_tmax_quadratic_719.png", height=4.5, width = 7.5, scale=.8, bg="white")

################################
# plot marginal effects

ddd$estimate[ddd$tmax==30 & ddd$gvi_mean==10]
ddd$estimate[ddd$tmax==30 & ddd$gvi_mean==30]

ggplot(ddd %>% filter(gvi_mean!="30"))+
  geom_tile(aes(x=as.factor(tmax), y=as.factor(gvi_mean ), fill=estimate))+
  geom_text(aes(x=as.factor(tmax), y=as.factor(gvi_mean ), label=round(estimate,0)))+
  scale_fill_distiller(name="kWh/month", palette = "Reds", direction = 1)+
  xlab("Monthly average Tmax")+
  ylab("Street green space (GVI)")+
  ggtitle("Predicted monthly electricity consumption")

ggsave("margins_heatmap_719.png", height=4.5, width = 7, scale=.8, bg="white")

#

ddd$estimate_pctg <- (ddd$estimate/mean(ddd$estimate)) - 1

ggplot(ddd %>% filter(gvi_mean!="30"))+
  geom_tile(aes(x=as.factor(tmax), y=as.factor(gvi_mean ), fill=estimate_pctg*100))+
  geom_text(aes(x=as.factor(tmax), y=as.factor(gvi_mean ), label=round(estimate_pctg,2)*100))+
  scale_fill_gradient2(name="%", low = "forestgreen", mid="white", high="darkred")+
  xlab("Monthly average Tmax")+
  ylab("Street Green Space (GVI)")+
  ggtitle("Predicted % deviation from average consumption")

ggsave("margins_heatmap_pctg_719.png", height=4.5, width = 6, scale=.8, bg="white")

############################

166/219
163/184

############################
############################
############################
# heterogeneity

ppp <- data.frame(dd$estimate, dd$gvi_mean, dd$tmax)

ppp$df.gvi_mean_c <- cut(ppp$dd.gvi_mean, c(10, 15, 20, 25, 30, 35))
ppp$df.tmax_c <- cut(ppp$dd.tmax, c(5, 10, 15, 20, 25, 30, 35))

ppp <- na.omit(ppp)

library(gg.layers)

boxplott <- ggplot(ppp)+
  gg.layers::geom_boxplot2(aes(x=df.tmax_c, y=dd.estimate, fill=df.gvi_mean_c), width.errorbar = 0.05)+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip_719.text.x = element_blank()
  )+
  xlab("Monthly average maximum temperature (°C)")+
  ylab("Predicted monthly electricity consumption (kWh)")+
  stat_summary(
    aes(x = df.tmax_c, y = dd.estimate, group = df.gvi_mean_c),
    fun = mean,
    geom = "point",
    shape = 21,
    size = 0.5,
    color = "red",
    fill = "transparent",
    position = position_dodge2(width = 0.8, preserve = "single" ))

ggsave("boxplots_urgs_tmax_719.png", boxplott, height=4.5, width = 6, scale=.8, bg="white")

###

# Fit ANOVA model

library(broom)

colnames(ppp)[1] <- c("pp.estimate")

aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(5,10]",]) %>% tidy()  -> anova_1
aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(10,15]",])  %>% tidy() -> anova_2
aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(15,20]",])  %>% tidy() -> anova_3
aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(20,25]",])  %>% tidy() -> anova_4
aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(25,30]",])  %>% tidy() -> anova_5
aov(pp.estimate ~ df.gvi_mean_c, data = ppp[ppp$df.tmax_c=="(30,35]",])  %>% tidy() -> anova_6

anova_res <- bind_rows(anova_1, anova_2, anova_3, anova_4, anova_5, anova_6)
anova_res$term <- c("Tmax (5,10]", "Residuals","Tmax (10,15]", "Residuals","Tmax (15,20]", "Residuals","Tmax (20,25]", "Residuals","Tmax (25,30]", "Residuals","Tmax (30,35]", "Residuals")

library(stargazer)

anova_res <- anova_res %>% mutate_if(is.numeric, round, 4)
row.names(anova_res) <- NULL
stargazer(anova_res, summary = F, type = "latex", out="anova_719.tex")

#############################################
#############################################
#############################################

# robustness specifications
#############

# clustering

v1 <- summary(reg_quadratic, cluster = ~pod)$se
v2 <- summary(reg_quadratic, cluster = ~comune)$se
v3 <- summary(reg_quadratic, cluster = ~comune+pod)$se
v4 <- summary(reg_quadratic, cluster = ~year)$se
v5 <- summary(reg_quadratic, cluster = ~year+comune)$se
v6 <- summary(reg_quadratic, cluster = ~year+comune+pod)$se

z1 <- summary(reg_quadratic, cluster = ~pod)$coefficients

###

v1_u <- z1 + 1.96 * v1
v1_l <- z1 - 1.96 * v1

v2_u <- z1 + 1.96 * v2
v2_l <- z1 - 1.96 * v2

v3_u <- z1 + 1.96 * v3
v3_l <- z1 - 1.96 * v3

v4_u <- z1 + 1.96 * v4
v4_l <- z1 - 1.96 * v4

v5_u <- z1 + 1.96 * v5
v5_l <- z1 - 1.96 * v5

v6_u <- z1 + 1.96 * v6
v6_l <- z1 - 1.96 * v6

##

vv <- data.frame(term = names(z1), estimate = z1, v1_u = v1_u, v1_l = v1_l, v2_u = v2_u, v2_l = v2_l, v3_u = v3_u, v3_l = v3_l, v4_u = v4_u, v4_l = v4_l, v5_u = v5_u, v5_l = v5_l, v6_u = v6_u, v6_l = v6_l)

vv <- reshape2::melt(vv, id.vars = c("term", "estimate"))

vv <- filter(vv, grepl("gvi_mean", vv$term))

vv <- filter(vv, term!="gvi_mean")

##

vv <- vv %>%
  mutate(cluster = case_when(
    grepl("v1", variable) ~ "pod",
    grepl("v2", variable) ~ "comune",
    grepl("v3", variable) ~ "comune+pod",
    grepl("v4", variable) ~ "year",
    grepl("v5", variable) ~ "year+comune",
    grepl("v6", variable) ~ "year+comune+pod"
  )) %>%
  mutate(type = case_when(
    grepl("_u", variable) ~ "upper",
    grepl("_l", variable) ~ "lower"
  )) %>%
  select(-variable)

vv <- pivot_wider(vv, names_from = type, values_from = value)

vv$term[vv$term=="hotmonths::1:tmax * gvi_mean"] <- "tmax * gvi_mean in hot months"
vv$term[vv$term=="hotmonths::1:I(tmax^2) * gvi_mean"] <- "tmax^2 * gvi_mean in hot months"

ggplot(vv, aes(x=cluster, y=estimate, color=cluster, group=cluster))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.5))+
  geom_point(aes(y=estimate))+
  facet_wrap(vars(term), scales = "free")+
  theme_classic()+
  theme(legend.position = "none")

ggsave("std_errors_clustering_robustness.png", width=9, height=4, scale=1.5)


###

dfs <- read_rds("processed_data.rds")
dfs2 <- dfs
dfs3 <- dfs 

dfs <- dfs[dfs$cons_719_month>50,]
dfs3 <- dfs2[dfs2$cons_719_month<=50,]

#

reg_quadratic1 <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=dfs, combine.quick = F)

reg_quadratic2 <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=dfs2, combine.quick = F)

reg_quadratic3 <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=dfs3, combine.quick = F)

etable(reg_quadratic2, reg_quadratic1, reg_quadratic3, cluster = ~comune+pod)

###

sink(file="reg_tab_50kwh.tex")
etable(reg_quadratic2, reg_quadratic1 , reg_quadratic3, cluster = ~comune+pod, tex=T, replace = T, headers=c("Full sample", ">50 kWh/month", "<=50 kWh/month"))
sink()

###

reg_quadratic_triple <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean * hotmonths - hotmonths| comune + pod + year + month + region^year, data=df, combine.quick = F)

etable(reg_quadratic, reg_quadratic_triple, cluster = ~comune+pod)

sink(file="reg_tab_triple.tex")
etable(reg_quadratic, reg_quadratic_triple, cluster = ~comune+pod, tex=T, replace = T)
sink()

###################################

# by comune type

reg_rural <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(type=="rural"), combine.quick = F)
summary(reg_rural, cluster = ~comune+pod)

reg_semiurban <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(type=="semiurban"), combine.quick = F)
summary(reg_semiurban, cluster = ~comune+pod)

reg_urban <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(type=="urban"), combine.quick = F)
summary(reg_urban, cluster = ~comune+pod)

# with reddito stratification

reg_reddito_q1 <- feols(log(cons_719_month) ~(tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(quartile_reddito=="Q1"), combine.quick = F)
summary(reg_reddito_q1, cluster = ~comune+pod)

reg_reddito_q2 <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(quartile_reddito=="Q2"), combine.quick = F)
summary(reg_reddito_q2, cluster = ~comune+pod)

reg_reddito_q3 <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(quartile_reddito=="Q3"), combine.quick = F)
summary(reg_reddito_q3, cluster = ~comune+pod)

reg_reddito_q4 <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df %>% filter(quartile_reddito=="Q4"), combine.quick = F)
summary(reg_reddito_q4, cluster = ~comune+pod)

etable(reg_rural, reg_semiurban, reg_urban, reg_reddito_q1, reg_reddito_q2, reg_reddito_q3, reg_reddito_q4, cluster = ~comune+pod)

sink(file="reg_tab2a_719.tex")
etable(reg_rural, reg_semiurban, reg_urban, cluster = ~comune+pod, tex=T, replace = T)
sink()

sink(file="reg_tab2b_719.tex")
etable( reg_reddito_q1, reg_reddito_q2, reg_reddito_q3, reg_reddito_q4, cluster = ~comune+pod, tex=T, replace = T)
sink()

########################################################

reg_covid <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean  |comune + pod + year+ year^covid + month + region^year, data=df, combine.quick = F)

reg_comunetrend <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean  |comune + pod + year + month + comune^year, data=df, combine.quick = F)

reg_provincetrend <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean  |comune + pod + year + month + province^year, data=df, combine.quick = F)

etable(reg_covid, reg_comunetrend, reg_provincetrend, cluster = ~comune+pod)

sink(file="reg_checks_covid_timetrend_719.tex")
etable(reg_covid, reg_comunetrend, reg_provincetrend, cluster = ~comune+pod, tex=T, headers = c("Covid", "Municipality time trend", "Provincial time trend"), replace = T)
sink()

###################################

# cooling degree hours

cdh <- read_rds("cdh_2020_2022_comuni.rds")
df <- merge(df, cdh, by=c("comune", "year", "month"))

#

# calculate weights

### https://ngreifer.github.io/WeightIt/index.html

tourism <- read.csv("tourism robustness/touristic_municipalities.csv")
tourism$X <- NULL
tourism$COD_PROV <- NULL

df_tour <- merge(df, tourism, by.x=c("comune"), by.y=c("COMUNE"), all.x=T)
df_tour$summer_touristic_destination <- ifelse(is.na(df_tour$summer_touristic_destination), 0, 1)

library(sf)
library(terra)
library(exactextractr)

comuni = read_sf("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/boundaries/Com01012023_g_WGS84.shp") %>% st_transform(4326) %>%  dplyr::select(PRO_COM, COMUNE)

lcz <- rast("C:/Users/falchetta/OneDrive - IIASA/IBGF_2024/implementation/climate/lcz/lcz_filter_v3.tif")

comuni_ee <- exact_extract(lcz, comuni, 'frac', progress = TRUE)
comuni_ee$COMUNE <- comuni$COMUNE

colnames(comuni_ee)[1:17] <- paste0("lcz_", colnames(comuni_ee)[1:17])

df_tour <- merge(df_tour, comuni_ee, by.x=c("comune"), by.y=c("COMUNE"))

builtage <- rast("GHS_AGE_1980102020_GLOBE_R2025A_54009_100_V1_0.tif")

comuni_builtage <-  exact_extract(builtage, comuni, 'frac', progress = TRUE)
colnames(comuni_builtage) <- paste0("builtage_", colnames(comuni_builtage))
comuni_builtage$COMUNE <- comuni$COMUNE

df_tour <- merge(df_tour, comuni_builtage, by.x=c("comune"), by.y=c("COMUNE"))

sf::sf_use_s2(F)

comuni_coord <- as.data.frame(st_coordinates(st_centroid(comuni)))
comuni_coord$COMUNE <- comuni$COMUNE

#

library(elevatr)

elevation_from_xy <- function(x, y, crs = 4326, z = 12, src = "aws") {
  stopifnot(length(x) == length(y))
  pts <- st_as_sf(data.frame(x = x, y = y), coords = c("x","y"), crs = crs, remove = FALSE)
  out <- get_elev_point(locations = pts, prj = st_crs(pts)$wkt, src = src, z = z, override_size_check = TRUE)
  # returns an sf with a column named 'elevation' (meters)
  st_drop_geometry(out)[, c("x","y","elevation")]
}

comuni_coord$elev <- elevation_from_xy(comuni_coord$X, comuni_coord$Y)$elevation

#

df_tour <- merge(df_tour, comuni_coord, by.x=c("comune"), by.y=c("COMUNE"))

#

ms <- as.data.frame(df_tour %>% dplyr::select(year, month, cons_719_month, tmax, tas, tmin, cdh, gvi_mean, pop, popdens, reddito_medio, builtage_frac_0, builtage_frac_1, builtage_frac_2, builtage_frac_3, builtage_frac_4, builtage_frac_5, elev))

ms$month <- as.numeric(ms$month)

ms <- na.omit(ms)

sum_tab <- data.frame(
  Min = apply(ms, 2, min),
  Q1  = apply(ms, 2, quantile, probs = 0.25),
  Median = apply(ms, 2, median),
  Mean = apply(ms, 2, mean),
  Q3  = apply(ms, 2, quantile, probs = 0.75),
  Max = apply(ms, 2, max),
  SD  = apply(ms, 2, sd)
)

library(xtable)

print(xtable(sum_tab, digits=1), file="sum_tab_pod_719.tex")

#

library(WeightIt)

df_match <- filter(df_tour, !is.na(reddito_medio))
df_match <- filter(df_match, !is.na(summer_touristic_destination))
df_match <- filter(df_match, !is.na(gvi_mean))
df_match <- filter(df_match, !is.na(tmax))
df_match <- filter(df_match, !is.na(popdens))
df_match <- filter(df_match, !is.na(cons_719_month))
df_match <- filter(df_match, !is.na(elev))

W <- weightit(gvi_mean ~ tmax +  popdens + reddito_medio + log(pop) + summer_touristic_destination + Y + builtage_frac_1 + builtage_frac_2 + builtage_frac_3 + builtage_frac_4 + builtage_frac_5 + elev, "ebal", data = df_match)

summary(W)

library(cobalt)

bal.tab(W, un = TRUE)

png("love_plot.png")
love.plot(W)
dev.off()

##

df_f <- filter(df_tour, !is.na(reddito_medio))
df_f <- filter(df_f, !is.na(summer_touristic_destination))
df_f <- filter(df_f, !is.na(gvi_mean))
df_f <- filter(df_f, !is.na(tmax))
df_f <- filter(df_f, !is.na(popdens))
df_f <- filter(df_f, !is.na(cons_719_month))
df_f <- filter(df_f, !is.na(elev))

reg_def5_w <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df_f, combine.quick = F, weights = W$weights)

reg_def5_unw <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod + year + month + region^year, data=df_f, combine.quick = F)

##

etable(reg_def5_w, reg_def5_unw, cluster = ~comune+pod)

sink(file="tab_matching.tex")
etable(reg_def5_w, reg_def5_unw, cluster = ~comune+pod, tex=T, label = "tab:matching", headers = c("Weighted", "Unweighted"))
sink()

##
##

dd_w <- predictions(reg_def5_w, variables=list(tmax=seq(5, 35, 5), gvi_mean=c(10, 15, 20, 25, 30)), transform = exp, vcov = F)
ddd_w <- dd_w %>% group_by(tmax, gvi_mean) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T))

dd_unw <- predictions(reg_def5_unw, variables=list(tmax=seq(5, 35, 5), gvi_mean=c(10, 15, 20, 25, 30)), transform = exp, vcov = F)
ddd_unw <- dd_unw %>% group_by(tmax, gvi_mean) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T))


p1 <- ggplot(ddd_w %>% filter(gvi_mean!="30"))+
  geom_line(aes(x=tmax, y=estimate, colour=as.factor(gvi_mean), group=as.factor(gvi_mean)))+
  scale_colour_brewer(palette = "Greens", name="GVI")+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip_719.text.x = element_blank()
  )+
  xlab("Monthly average maximum temperature (°C)")+
  ylab("Predicted monthly electricity consumption (kWh)")+
  ylim(c(150, 200))+
  xlim(c(20,35))

p2 <- ggplot(ddd_unw %>% filter(gvi_mean!="30"))+
  geom_line(aes(x=tmax, y=estimate, colour=as.factor(gvi_mean), group=as.factor(gvi_mean)))+
  scale_colour_brewer(palette = "Greens", name="GVI")+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip_719.text.x = element_blank()
  )+
  xlab("Monthly average maximum temperature (°C)")+
  ylab("Predicted monthly electricity consumption (kWh)")+
  ylim(c(150, 200))+
  xlim(c(20,35))

#

library(patchwork)

p1 + p2

ggsave("lines_urgs_tmax_quadratic_719_weighting.png", height=4.5, width = 7.5, scale=1.1, bg="white")

###
###

# hotcold

dfhotcold <- df %>% filter(hotmonths==1) %>%  group_by(comune) %>% dplyr::summarise(tmax=mean(tmax, na.rm=T))

reg_quadratic_cold <- feols(log(cons_719_month) ~  tmax:hotmonths  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfhotcold$comune[dfhotcold$tmax<27.73]), combine.quick = F)

reg_quadratic_hot <- feols(log(cons_719_month) ~  tmax:hotmonths  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfhotcold$comune[dfhotcold$tmax>=27.73]), combine.quick = F)

#

reg_quadratic_cold_summer_gvi <- feols(log(cons_719_month) ~  tmax*gvi_mean  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfhotcold$comune[dfhotcold$tmax<27.73]), combine.quick = F)

reg_quadratic_hot_summer_gvi <- feols(log(cons_719_month) ~  tmax*gvi_mean  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfhotcold$comune[dfhotcold$tmax>=27.73]), combine.quick = F)

etable(reg_quadratic_cold, reg_quadratic_hot, reg_quadratic_cold_summer_gvi, reg_quadratic_hot_summer_gvi, cluster = ~comune+pod)

sink(file="reg_hotcold.tex")
etable(reg_quadratic_cold, reg_quadratic_hot, reg_quadratic_cold_summer_gvi, reg_quadratic_hot_summer_gvi, cluster = ~comune+pod, tex=T, replace = T, headers = c("Municipalities where summer avg. Tmax < median", "Municipalities where summer avg. Tmax > median", "Municipalities where summer avg. Tmax < median, GVI role", "Municipalities where summer avg. Tmax > median, GVI role"))
sink()

##########

# greenness level

dfgreennogreen <- df %>%  group_by(comune) %>% dplyr::summarise(gvi_mean=mean(gvi_mean, na.rm=T))

reg_quadratic_littlegreen <- feols(log(cons_719_month) ~  tmax:hotmonths  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfgreennogreen$comune[dfgreennogreen$gvi_mean<21.327]), combine.quick = F)

reg_quadratic_highgreen <- feols(log(cons_719_month) ~  tmax:hotmonths  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfgreennogreen$comune[dfgreennogreen$gvi_mean>=21.327]), combine.quick = F)

#

reg_quadratic_littlegreen_summer_gvi <- feols(log(cons_719_month) ~  tmax*gvi_mean  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfgreennogreen$comune[dfgreennogreen$gvi_mean<21.327]), combine.quick = F)

reg_quadratic_highgreen_summer_gvi <- feols(log(cons_719_month) ~  tmax*gvi_mean  | comune + pod + year + month + region^year, data=df %>% filter(comune %in% dfgreennogreen$comune[dfgreennogreen$gvi_mean>=21.327]), combine.quick = F)

etable(reg_quadratic_littlegreen, reg_quadratic_highgreen, reg_quadratic_littlegreen_summer_gvi, reg_quadratic_highgreen_summer_gvi, cluster = ~comune+pod)

sink(file="reg_greennogreen.tex")
etable(reg_quadratic_littlegreen, reg_quadratic_highgreen, reg_quadratic_littlegreen_summer_gvi, reg_quadratic_highgreen_summer_gvi, cluster = ~comune+pod, tex=T, replace = T, headers = c("Municipalities where GVI < median", "Municipalities where GVI > median", "Municipalities where GVI < median, GVI role", "Municipalities where GVI > median, GVI role"))
sink()

##########

library(stargazer)

ms_g <- as.data.frame(df %>% dplyr::select(cons_719_month, comune, tmax, tas, tmin, cdh, gvi_mean, pop, popdens, reddito_medio))
ms_g <- group_by(ms_g, comune) %>% dplyr::summarise_all(mean, na.rm=T)

ms_g <- mutate_if(ms_g, is.numeric, round, 2)

library(knitr)
library(kableExtra)
ms_g <- kable(ms_g, "latex", longtable = T, booktabs = T) %>%
  kable_styling(latex_options = c("repeat_header"), font_size = 10) %>%
  landscape()
writeLines(ms_g, 'sum_comune_mean.tex')


#

# cdd
reg_def1 <- feols(log(cons_719_month) ~ CDD * gvi_mean | comune + pod + year + month + region^year, data=df, combine.quick = F)

# cdh

reg_def3 <- feols(log(cons_719_month) ~  cdh * gvi_mean  | comune + pod + year + month + region^year, data=df, combine.quick = F)

# bins specification

reg_def4 <- feols(log(cons_719_month) ~ (Q_botom_tmax + Q1_tmax + Q2_tmax + Q4_tmax + Q5_tmax + Q_top_tmax) * gvi_mean | comune + pod + year + month + region^year, data=df)

etable(reg_def1, reg_def3, reg_def4, cluster = ~comune+pod)

sink(file="tab_cdd_bins_719.tex")
etable(reg_def1, reg_def2, reg_def3, reg_def4, cluster = ~comune+pod, tex=T, replace = T)
sink()

###################################

# tourism robustness

tourism <- read.csv("tourism robustness/touristic_municipalities.csv")
tourism$X <- NULL
tourism$COD_PROV <- NULL

df_tour <- merge(df, tourism, by.x=c("comune"), by.y=c("COMUNE"), all.x = T)
df_tour$summer_touristic_destination <- ifelse(is.na(df_tour$summer_touristic_destination), 0, 1)

reg_nontourism <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean  | comune + pod + year + month + region^year, data=df_tour[df_tour$summer_touristic_destination==0,], combine.quick = F)

reg_tourism <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean  | comune + pod + year + month + region^year, data=df_tour[df_tour$summer_touristic_destination==1,], combine.quick = F)


etable(reg_tourism, reg_nontourism, cluster = ~comune+pod)

sink(file="tab_tourism_719.tex")
etable(reg_tourism, reg_nontourism, cluster = ~comune+pod, tex=T, label = "tab:tourism", headers = c("Touristic municipalities", "Non-touristic municipalities"), replace = T)
sink()

########
# monthly heterogeneity

outer <- list()

for(month_i in 1:12){
  
  print(month_i)
  
  reg_quadratic_monthly <- feols(log(cons_719_month) ~  (tmax + tmax^2) * gvi_mean | comune + pod +  year + region^year, data=df %>% filter(month == month_i), combine.quick = F)
  
  dd <- predictions(reg_quadratic_monthly, variables=list(tmax=round(seq(quantile(df$tmax[df$month==month_i], c(0.05, 0.95))[1], quantile(df$tmax[df$month==month_i], c(0.05, 0.95))[2], 1)), gvi_mean=c(10, 15, 20, 25)), transform = exp, vcov = F)
  
  ddd <- dd %>% group_by(tmax, gvi_mean) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T))
  
  outer[[month_i]] <- ggplot(ddd)+
    geom_line(aes(x=tmax, y=estimate, colour=as.factor(gvi_mean), group=as.factor(gvi_mean)))+
    scale_colour_brewer(palette = "Greens", name="GVI")+
    scale_fill_brewer(palette = "Greens", name="GVI")+
    theme_classic()+
    theme(
      strip.background = element_blank(),
      strip_719.text.x = element_blank()
    )+
    xlab("")+
    ylab("")+
    ggtitle(month.abb[month_i])+
    ylim(c(145,245))
  
}

library(patchwork)

plottone <- wrap_plots(outer, ncol = 3) + plot_layout(guides = "collect")

ggsave("lines_urgs_tmax_quadratic_719_monthly.png", plottone, height=10, width = 7.5, scale=1, bg="white")

###################################
###################################
# import a comune-level (collapsed) version of the data

df_comuni <- read_rds("processed_data_comuni.rds")

df_comuni_distr <- df_comuni
df_comuni_distr$tmax <- cut(df_comuni_distr$tmax, c(seq(10,35, by=5)))
df_comuni_distr$gvi_mean <- cut(df_comuni_distr$gvi_mean, c(c(10, 15, 20, 25, 30)))

df_comuni_freq <- df_comuni_distr %>% ungroup() %>% 
  dplyr::count(tmax, gvi_mean) %>%
  dplyr::mutate(freq = n / sum(n))

df_comuni_freq <- na.omit(df_comuni_freq)

ggplot(df_comuni_freq) +
  geom_tile(aes(x = tmax, y = gvi_mean, fill = freq*100)) +
  geom_text(aes(x = tmax, y = gvi_mean, label = n))+
  scale_fill_distiller(name="%", palette = "Reds", direction = 1)+
  xlab("Monthly average Tmax")+
  ylab("Street Green Space (GVI)")+
  ggtitle("Frequency of observations in the municipality-level dataset")

ggsave("distribution_heatmap_comuni.png", height=4.5, width = 6, scale=.8, bg="white")

#

library(stargazer)

ms <- as.data.frame(df_comuni %>% ungroup() %>% dplyr::select(cons_719_month, tmax, gvi_mean))

ms <- na.omit(ms)

sum_tab <- data.frame(
  Min = apply(ms, 2, min),
  Q1  = apply(ms, 2, quantile, probs = 0.25),
  Median = apply(ms, 2, median),
  Mean = apply(ms, 2, mean),
  Q3  = apply(ms, 2, quantile, probs = 0.75),
  Max = apply(ms, 2, max),
  SD  = apply(ms, 2, sd)
)

library(xtable)

print(xtable(sum_tab, digits=1), file="sum_tab_comune_719.tex")


hist(df_comuni$gvi_mean[df_comuni$year==2022], main="", xlab="GVI")

# df_comuni <- df_comuni %>% group_by(comune) %>% mutate(gvi_mean=mean(gvi_mean, na.rm = T)) #### constant treatment?

# set hot months categorical variable

df_comuni$hotmonths <- as.factor(ifelse(as.numeric(df_comuni$month)>5 & as.numeric(df_comuni$month)<9, 1, 0))

df_comuni$quartile_reddito <- cut(df_comuni$reddito_medio, quantile(df_comuni$reddito_medio, by=seq(0, 1, 0.2), na.rm = T), labels = c("Q1", "Q2", "Q3", "Q4"))

# test some comune-level specifications

# preliminary question: do Tmax variations across months affect green space?
reg_def1 <- feols(tmax ~ gvi_mean:month  | region + month + year, data=df_comuni)
summary(reg_def1, cluster = ~comune) #YES, always negatively (hotter climate = less green space)

reg_def2 <- feols(tmax ~ gvi_mean | region + month + year, data=df_comuni)
summary(reg_def2, cluster = ~comune) #YES, always negatively (hotter climate = less green space)

etable(reg_def2, reg_def1, vcov="cluster")
etable(reg_def2, reg_def1, vcov="cluster", file = "regtab1_ITA_across_comunelevel_719.tex", tex = T, replace = T)


# at the comune level, do Tmax variations across months affect electricity consumption?
reg_def3 <- feols(log(cons_719_month) ~ tmax:month |  month + year, data=df_comuni, combine.quick = F)
summary(reg_def3, cluster = ~comune) #YES, positively in Summer months 

##

# consumption regressions

# at the comune level, does UGS density mediate the positive effect of Tmax on electricity consumption in the Summer months?
reg_def2a <- feols(log(cons_719_month) ~ tmax + gvi_mean + gvi_mean:tmax |  region + month + year, data=df_comuni, combine.quick = F)
summary(reg_def2a, cluster = ~comune)

reg_quadratic <- feols(log(cons_719_month) ~ (tmax + tmax^2) * gvi_mean |  comune + region + month + year, data=df_comuni, combine.quick = F)
summary(reg_quadratic, cluster = ~comune)

reg_def_urban <- feols(log(cons_719_month) ~ tmax + gvi_mean + gvi_mean:tmax | region + month + year, data=df_comuni[df_comuni$popdens>250,]) #, combine.quick = F
summary(reg_def_urban, cluster = ~comune)

reg_def_rural <- feols(log(cons_719_month) ~  tmax + gvi_mean + gvi_mean:tmax |  region + month + year, data=df_comuni[df_comuni$popdens<250,]) #, combine.quick = F
summary(reg_def_rural, cluster = ~comune)

# stratifica per reddito

reg_def_wealth_q1 <- feols(log(cons_719_month) ~  tmax + gvi_mean + gvi_mean:tmax  |  region + month + year, data=df_comuni[df_comuni$quartile_reddito=="Q1",])
reg_def_wealth_q2 <- feols(log(cons_719_month) ~  tmax + gvi_mean + gvi_mean:tmax  |  region + month + year, data=df_comuni[df_comuni$quartile_reddito=="Q2",])
reg_def_wealth_q3 <- feols(log(cons_719_month) ~  tmax + gvi_mean + gvi_mean:tmax  |  region + month + year, data=df_comuni[df_comuni$quartile_reddito=="Q3",])
reg_def_wealth_q4 <- feols(log(cons_719_month) ~  tmax + gvi_mean + gvi_mean:tmax  |  region + month + year, data=df_comuni[df_comuni$quartile_reddito=="Q4",])

etable(reg_def2a, reg_def_urban, reg_def_rural, cluster = ~comune)
etable(reg_def_wealth_q1, reg_def_wealth_q2, reg_def_wealth_q3, reg_def_wealth_q4, cluster = ~comune)

sink(file="tab_citylevel_719.tex")
etable(reg_quadratic, cluster = ~comune, tex=T, replace = T)
sink()

sink(file="tab_citylevel2_719.tex")
etable(reg_def_wealth_q1, reg_def_wealth_q2, reg_def_wealth_q3, reg_def_wealth_q4, cluster = ~comune, tex=T, replace = T)
sink()

#############################
#############################
#############################

source("policy_analysis.R")

#############################
#############################
#############################
