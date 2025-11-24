
# https://www.istat.it/classificazione/classificazione-dei-comuni-in-base-alla-densita-turistica/

library(readxl)

setwd("F:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/systemic cooling poverty/glob_assess_energy_demand_reduction_potential/seme_data/tourism robustness/")

tourism <- read.csv("Classificazioni-turistiche_comuni_italiani_19_01_2022.csv", sep=";")

tourism1 <- tourism[grepl("lacuale", tourism$CATEGORIE.TURISTICHE.PRESENTI),]
tourism2 <- tourism[grepl("marittima", tourism$CATEGORIE.TURISTICHE.PRESENTI),]
tourism3 <- tourism[grepl("montana", tourism$CATEGORIE.TURISTICHE.PRESENTI),]

tourism <- bind_rows(tourism1, tourism2, tourism3)
tourism <- tourism[!duplicated(tourism), ]

tourism$summer_touristic_destination <- 1

tourism <- dplyr::select(tourism, COMUNE, COD_PROV, summer_touristic_destination)

write.csv(tourism, "touristic_municipalities.csv")
