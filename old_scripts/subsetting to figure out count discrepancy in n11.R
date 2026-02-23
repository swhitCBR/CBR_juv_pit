
library(dplyr)

# from "04a_recomp_to_rep_m_or_b_scripts98_25_wPD568"
# lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
# lgr_est_outDF2 <- readRDS("temp/lgr_est_outDF2.rds")
lgr_est_outDF3 <- readRDS("temp/lgr_est_outDF3.rds")
table(lgr_est_outDF3$esutype)
head(lgr_est_outDF3)

lgr_est_outDF3 %>% filter(esutype=="SR_Ch1" & defin_det_yr==2013 & aggre_lev == "year" & reartype=="W") 





lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
subb_tb <- lgr_dh_tab2 %>% filter(esutype=="SR_Ch1" & defin_det_yr==2013 )  #%>% mutate(DH_code_short=substr(DH_code,3,5))

library(dplyr)
subb_tb %>% group_by(esutype,defin_det_yr,virt_det,DH_code) %>% summarize(ntags=length(unique(code)))

subb_tb %>% group_by(esutype,defin_det_yr,dat_grp) %>% summarize(ntags=length(unique(code)))
subb_tb %>% group_by(esutype,defin_det_yr,reartype,dat_grp) %>% summarize(ntags=length(unique(code)))

#confusing
subb_tb %>% group_by(esutype,defin_det_yr,reartype,dat_grp,DH_code) %>% summarize(ntags=length(unique(code))) %>% tidyr::pivot_wider(names_from=DH_code,values_from=ntags)


subb_tb %>% 
  filter(reartype=="W" & dat_grp=="lgr_pooled") %>%
  group_by(esutype,defin_det_yr,reartype,dat_grp,DH_code) %>%
  summarize(ntags=length(unique(code))) %>% 
  tidyr::pivot_wider(names_from=DH_code,values_from=ntags)


subb_tb %>% 
  filter(reartype=="W" & dat_grp=="lgr_pooled") %>%
  mutate(DH_label_mod=gsub(DH_label,pattern="LGR -> ",replacement="")) %>%
  group_by(esutype,defin_det_yr,reartype,dat_grp,DH_label_mod) %>%
  summarize(ntags=length(unique(code))) %>% 
  tidyr::pivot_wider(names_from=DH_label_mod,values_from=ntags)
