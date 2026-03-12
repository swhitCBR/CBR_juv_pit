library(dplyr)

lgr_dh_tab_l <- readRDS("comp_files/lgr_dh_tab_wMCNPOOL.rds")

lgr_dh_tab_year <- lgr_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

lgr_dh_tab_month <- lgr_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,month,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,month,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

lgr_dh_tab_weeks2 <- lgr_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,weeks2,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,weeks2,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

lgr_dh_tab_week1 <- lgr_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,week1,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,week1,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

lgr_dh_tab_days3.5 <- lgr_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,days3.5,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,days3.5,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

lgr_dh_tab_day <- lgr_dh_tab_l%>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,day,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,day,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

###################### #
# combining lgr bins
###################### #

lgr_bin_brkDF <- bind_rows(
  lgr_dh_tab_year %>% mutate(binsize="year") ,
  lgr_dh_tab_month %>% mutate(binsize="month") ,
  lgr_dh_tab_weeks2 %>% mutate(binsize="weeks2") ,
  lgr_dh_tab_week1 %>% mutate(binsize="week1") ,
  lgr_dh_tab_days3.5 %>% mutate(binsize="days3.5"),
  lgr_dh_tab_day %>% mutate(binsize="day") ) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

saveRDS(lgr_bin_brkDF,"comp_files/lgr_bin_brkDF_wMCNPOOL.rds")



library(dplyr)

mcn_dh_tab_l <- readRDS("comp_files/mcn_dh_tab_wMCNPOOL.rds")

mcn_dh_tab_year <- mcn_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

mcn_dh_tab_month <- mcn_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,month,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,month,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

mcn_dh_tab_weeks2 <- mcn_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,weeks2,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,weeks2,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

mcn_dh_tab_week1 <- mcn_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,week1,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,week1,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

mcn_dh_tab_days3.5 <- mcn_dh_tab_l %>% 
  arrange(dat_grp,esutype,reartype,defin_det_yr,days3.5,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,days3.5,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

mcn_dh_tab_day <- mcn_dh_tab_l%>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,day,cell_count) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,day,cell_count) %>%
  summarize(value=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=cell_count) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

###################### #
# combining mcn bins
###################### #

mcn_bin_brkDF <- bind_rows(
  mcn_dh_tab_year %>% mutate(binsize="year") ,
  mcn_dh_tab_month %>% mutate(binsize="month") ,
  mcn_dh_tab_weeks2 %>% mutate(binsize="weeks2") ,
  mcn_dh_tab_week1 %>% mutate(binsize="week1") ,
  mcn_dh_tab_days3.5 %>% mutate(binsize="days3.5"),
  mcn_dh_tab_day %>% mutate(binsize="day") ) %>%
  mutate(n.11=ifelse(is.na(n.11),0,n.11),
         n.11greq10=n.11>=10)

saveRDS(mcn_bin_brkDF,"comp_files/mcn_bin_brkDF_wMCNPOOL.rds")
