
library(dplyr)


obs_rel_grps10 <- readRDS("comp_files/obs_rel_grps10.rds")

table(obs_rel_grps10$AVIAN_recov)
obs_rel_grps8 <- readRDS("comp_files/obs_rel_grps8.rds")
table(obs_rel_grps10$dup_obs)

lgr_obs_rel_forDH <- obs_rel_grps10 %>% 
  filter(dat_grp %in% c("lgr_pooled","lgr_det")) %>%
  mutate(loc_consid=ifelse(prim_loc_cat %in% c("BON","JDA","Estuary"),"BONplus",prim_loc_cat))


lgr_loc_considered <- c("LGR","MCN","BONplus")

# obs_rel_grps9 <- readRDS("comp_files/obs_rel_grps9.rds")
################################################################### #
# # CONVERT TO ANTENNA LEVEL VERSION
# lgr_dh_tabANT <- lgr_obs_rel_forDH %>%
#   filter(loc_consid %in% lgr_loc_considered) %>% # only considering certain locations for model
#   group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
#   summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
#             AVIAN_recov=any(AVIAN_recov),
#             within=any(within)) %>%
#   mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
#          DH_label=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label_ant))
################################################################### #

lgr_dh_tab <- lgr_obs_rel_forDH %>% 
  filter(within) %>%
  filter(loc_consid %in% lgr_loc_considered) %>% # only considering certain locations for model
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(
    DH_label_ant=paste(loc_consid,collapse=" -> "),
    AVIAN_recov=any(AVIAN_recov),
    within=any(within),
    event=unique(event)) %>%
  mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> BONplus",
         DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> BONplus"),DH_label_ant)) 



################################################################### #
# recursive relabeling to form 3-occasion groupings
################################################################### #

# lgr_dh_tab$DH_label <- lgr_dh_tab$DH_label_orig
DH_label_LGR_ls <- list()

DH_label_LGR_ls[[1]] <- sort(table(lgr_dh_tab$DH_label),decreasing=T)

lgr_dh_tab$DH_label1 <-  gsub(lgr_dh_tab$DH_label,pattern="LGR -> LGR",replacement="LGR"); DH_label_LGR_ls[[2]] <- sort(table(lgr_dh_tab$DH_label1),decreasing=T)
lgr_dh_tab$DH_label2 <-  gsub(lgr_dh_tab$DH_label1,pattern="MCN -> MCN",replacement="MCN"); DH_label_LGR_ls[[3]] <- sort(table(lgr_dh_tab$DH_label2),decreasing=T)
lgr_dh_tab$DH_label3 <- gsub(lgr_dh_tab$DH_label2,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[4]] <- sort(table(lgr_dh_tab$DH_label3),decreasing=T)
lgr_dh_tab$DH_label4 <- gsub(lgr_dh_tab$DH_label3,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[5]] <- sort(table(lgr_dh_tab$DH_label4),decreasing=T)
lgr_dh_tab$DH_label5 <- gsub(lgr_dh_tab$DH_label4,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[6]] <- sort(table(lgr_dh_tab$DH_label5),decreasing=T)
lgr_dh_tab$DH_label6 <- gsub(lgr_dh_tab$DH_label5,pattern="LGR -> BONplus -> MCN -> BONplus",replacement="LGR -> BONplus"); DH_label_LGR_ls[[7]] <- sort(table(lgr_dh_tab$DH_label6),decreasing=T)
lgr_dh_tab$DH_label7 <- gsub(lgr_dh_tab$DH_label6,pattern="LGR -> BONplus -> MCN",replacement="LGR -> BONplus"); DH_label_LGR_ls[[8]] <- sort(table(lgr_dh_tab$DH_label7),decreasing=T)
# lgr_dh_tab$DH_label8 <-  gsub(lgr_dh_tab$DH_label7,pattern="BONplus",replacement="BON"); DH_label_LGR_ls[[9]] <- sort(table(lgr_dh_tab$DH_label8),decreasing=T)

lgr_dh_tab$DH_label_orig <- lgr_dh_tab$DH_label
lgr_dh_tab$DH_label <- lgr_dh_tab$DH_label7

DH_label_LGR_ls


head(data.frame(lgr_dh_tab))
lgr_dh_tab2 <- lgr_dh_tab %>% select(-DH_label1,-DH_label2,-DH_label3,-DH_label4,-DH_label5,-DH_label6,-grp_code,-code)
#  # mutate(n.11='LGR -> MCN -> BON',
  #        n.10='LGR -> MCN',
  #        n.01='LGR -> BON',
  #        n.00='LGR')

lgr_dh_tab2 <- lgr_dh_tab2 %>% select(-DH_label_ant)
lgr_dh_tab2 <- lgr_dh_tab2 %>% select(-DH_label7)

# head(data.frame(obs_rel_grps8))
# table(duplicated(obs_rel_grps8$code2))
# nrow(obs_rel_grps8)
# obs_rel_grps8 %>% filter(tagid)

duplgr_tgs <- obs_rel_grps8$tagid[duplicated(obs_rel_grps8$tagid)]

# obs_rel_grps8 %>% filter(tagid %in% duplgr_tgs[1])

lgr_dh_tab3 <- lgr_dh_tab2 %>% 
  left_join(obs_rel_grps8 %>% filter(!duplicated(tagid)) %>% 
              select(tagid,day,days3.5,week1,weeks2,month)) 

  
lgr_dh_tab3$cell_count <- c("n.00","n.10","n.01","n.11")[match(lgr_dh_tab3$DH_label,c("LGR","LGR -> MCN","LGR -> BONplus","LGR -> MCN -> BONplus"))]
# lgr_dh_tab3 <- lgr_dh_tab3 %>% mutate()




mcn_loc_considered <- c("MCN","BON","Estuary")

mcn_obs_rel_forDH <- obs_rel_grps10 %>% 
  filter(dat_grp %in% c("mcn_det") & prim_loc_cat %in% mcn_loc_considered) %>% 
  mutate(loc_consid=prim_loc_cat)

table(mcn_obs_rel_forDH$prim_loc_cat)

mcn_dh_tab <- mcn_obs_rel_forDH %>% 
  filter(loc_consid %in% mcn_loc_considered) %>% # only considering certain locations for model
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
            AVIAN_recov=any(AVIAN_recov),
            within=any(within),
            event=unique(event)) %>%
  mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
         DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> Estuary"),DH_label_ant)) 

table(sort(mcn_dh_tab$DH_label,decreasing=T))

mcn_dh_tab2 <- mcn_dh_tab %>% 
  ungroup() %>%
  select(-grp_code,-code)
table(mcn_dh_tab2$DH_label)

mcn_dh_tab3 <- mcn_dh_tab2 %>% 
  left_join(obs_rel_grps8 %>% 
              select(tagid,day,days3.5,week1,weeks2,month)) 


mcn_dh_tab3$cell_count <- c("n.00","n.10","n.01","n.11")[match(mcn_dh_tab3$DH_label,c("MCN","MCN -> BON","MCN -> Estuary","MCN -> BON -> Estuary"))]


saveRDS(mcn_dh_tab3,"comp_files/mcn_dh_tab.rds")
saveRDS(lgr_dh_tab3,"comp_files/lgr_dh_tab.rds")

