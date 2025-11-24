
reg_quadratic <- feols(log(cons_719_month) ~ tmax + tmax^2 + gvi_mean + i(hotmonths, tmax, keep="1") + i(hotmonths, tmax*gvi_mean, keep="1") + i(hotmonths, tmax^2, keep="1") + i(hotmonths, tmax^2*gvi_mean, keep="1")  | comune + pod + year + month + region^year + hotmonths, data=df, combine.quick = F)

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
