
df_s <- group_by(df, region, month, year) %>% dplyr::summarise(cons_mean=mean(cons_month, na.rm=T), cons_sd=sd(cons_month, na.rm=T))

df_s$date <- as.Date(paste("01", ifelse(nchar(as.character(df_s$month))==1, paste0("0", df_s$month), df_s$month), df_s$year, sep="-"), format="%d-%m-%Y")

df_s$hotmonths <- as.factor(ifelse(as.numeric(as.character(df_s$month))>5 & as.numeric(as.character(df_s$month))<9, "hot", "not hot"))

ggplot(df_s)+
  geom_ribbon(aes(x=date, ymin=cons_mean-cons_sd, ymax=cons_mean+cons_sd), alpha=0.1)+
  geom_line(aes(x=date, y=cons_mean))+
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y")+
  facet_wrap(vars(region))
