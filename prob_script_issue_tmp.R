# mcn_dh_tab2 <- readRDS("comp_files/mcn_dh_tab2_9825_wPD568.rds")

library(dplyr)

dh_subb <- readRDS("comp_files/dh_subb.rds")
dh_tab <-readRDS("comp_files/dh_tab.rds")

lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
mcn_dh_tab2 <- readRDS("comp_files/mcn_dh_tab2_9825_wPD568.rds")


lgr_naDF <- lgr_dh_tab2[is.na(lgr_dh_tab2$DH_code),]
nrow(lgr_dh_tab2[!is.na(lgr_dh_tab2$DH_code),])


sort(table(lgr_naDF$DH_label_mod),decreasing = T)


head(lgr_dh_tab2)
lgr_dh_tab2[lgr_dh_tab2$DH_label_mod==" -> Estuary",]
