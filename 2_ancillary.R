
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

##

outer <- read_rds("2_ancillary_analysis_data.rds")

####

library(stargazer)

ms <- as.data.frame(outer %>%  dplyr::select(value, variable, out_b, build_h, pop_dens, water, elevation))
ms = ms %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    names_prefix = "tmax_"
  )

stargazer(as.data.frame(ms), summary = T, out="F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/sum_tab_points.tex")

#

outer <- dplyr::group_by(outer, city, variable) %>% dplyr::mutate(value_citymean=mean(value, na.rm=T))
outer$id <- paste0(outer$x, outer$y)

####

outer$out_b_cut <- cut(outer$out_b, quantile(outer$out_b, seq(0, 1, 0.2)))

m0 <- feols(value ~ out_b : value_citymean  + value_citymean |  id + variable , data=outer, 
            cluster = ~  id + variable)

m1 <- feols(value ~ out_b : value_citymean  + out_b^2 : value_citymean + value_citymean |  id + variable , data=outer, 
            cluster = ~  id + variable)

m2 <- feols(value ~ out_b_cut : value_citymean + value_citymean |  id + variable , data=outer, 
            cluster = ~  id + variable)

m3 <- feols(value ~ out_b + pop_dens + elevation + water | city + variable , data=outer, 
            cluster = ~ id + variable)

etable(m0, m1, m2, m3)

etable(m0, m1, m2, m3, cluster = ~ id + variable , file = paste0("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/", "reg_ancillary.tex"), tex = T, replace = T)

####
###

mm <- marginaleffects::predictions(m0, vcov=F, variables=list(value_citymean=seq(20, 35, 5), out_b=seq(10, 30, 5)))

mmm <- mm %>% group_by(value_citymean, out_b) %>% dplyr::summarise(estimate=mean(estimate, na.rm=T))

ggplot(mmm)+
  geom_col(aes(x=1, y=100*(estimate/value_citymean), fill=as.factor(out_b), group=as.factor(out_b)), position = "dodge")+
  scale_colour_brewer(palette = "Greens", name="GVI")+
  scale_fill_brewer(palette = "Greens", name="GVI")+
  theme_classic()+
  theme(
    strip.background = element_blank(),
    strip_719.text.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+ 
  coord_cartesian(ylim=c(90, 100))+
  ggtitle(("Fraction of city-level average warming reflected at the grid cell-level"))+
  geom_hline(yintercept=100)+
  xlab("")+
  ylab("%")

ggsave("within_city_cooling.png", height=4.5, width = 7, scale=.8, bg="white")

##

library(EnvStats)

gg <- expand.grid(X=1:12, Y=unique(outer$city))

ggg <- mapply(function(X, Y){ IQR(outer$value[as.numeric(as.character(outer$variable))==X & outer$city==Y])},gg$X, gg$Y)

gg$IQR <- ggg

ggplot(gg)+
  geom_tile(aes(x=Y, y=as.factor(X), fill=IQR))+
  geom_text(aes(x=Y, y=as.factor(X), label=round(IQR, 2)))+
  scale_fill_distiller(palette = "YlOrRd")+
  xlab("City")+
  ylab("Month")

ggsave("within_city_t_IQR.png", height=4.5, width = 7, scale=.8, bg="white")

