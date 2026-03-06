library(dplyr)


obs_rel_grps8 <- readRDS("comp_files/obs_rel_grps8.rds")
obs_rel_grps10 <- readRDS("comp_files/obs_rel_grps10.rds")

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

mcn_dh_tab3 <- mcn_dh_tab2 %>% left_join(obs_rel_grps8 %>% select(tagid,day,days3.5,week1,weeks2,month))
























mcn_dh_tab2_w <- mcn_dh_tab2 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,DH_label) #%>% 
  # summarize(value=length(unique(tagid))) %>% 
  # mutate(
  #   n.11='MCN -> BON -> Estuary',
  #   n.10='MCN -> BON',
  #   n.01='MCN -> Estuary',
  #   n.00='MCN')




mcn_dh_tab2_w <- mcn_dh_tab2 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>% 
  summarize(value=length(unique(tagid))) %>%
  mutate(DH_label=factor(DH_label,levels=c('MCN -> BON -> Estuary','MCN -> BON','MCN -> Estuary','MCN'))) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>%
  tidyr::pivot_wider(names_from=DH_label) #%>%
  # mutate(
  #   n.11='MCN -> BON -> Estuary',
  #   n.10='MCN -> BON',
  #   n.01='MCN -> Estuary',
  #   n.00='MCN')



############################################## #
# individual tags converted to cell counts
############################################## #

mcn_dh_tab2_w <- mcn_dh_tab2 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>% 
  summarize(value=length(unique(tagid))) %>%
  mutate(DH_label=factor(DH_label,levels=c('MCN -> BON -> Estuary','MCN -> BON','MCN -> Estuary','MCN'))) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>%
  tidyr::pivot_wider(names_from=DH_label)

mcn_dh_tab3_w <- mcn_dh_tab2_w %>% 
  rename(n.11='MCN -> BON -> Estuary',
         n.10='MCN -> BON',
         n.01='MCN -> Estuary',
         n.00='MCN')

saveRDS(mcn_dh_tab3_w,"comp_files/mcn_dh_tab3_w.rds")


source("C:/repos/repo_simCJS/simCJS/R/per2_surph_ests.R")
source("C:/repos/repo_simCJS/simCJS/R/get_est_tab.R")
# View(cbind(mcn_dh_tab3_w,get_est_tab(df_in = mcn_dh_tab3_w)))

mcn_det_dh <- mcn_dh_tab3_w %>% filter(dat_grp=="mcn_det")
mcn_det_dh %>% filter(esutype=="SR_Sock")


mcn_det_dh %>% filter(esutype=="SR_Sock") %>%   
  group_by(dat_grp,esutype,defin_det_yr) %>% 
  summarize(n.11=sum(n.11,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.00=sum(n.00,na.rm=T))

# ggplot2::ggplot(data=mcn_det_dh,aes(y=value,x=defin_det_yr)


sock_pltdat <- mcn_det_dh %>% filter(esutype=="SR_Sock") %>%   
  group_by(dat_grp,esutype,defin_det_yr) %>% 
  summarize(n.11=sum(n.11,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.00=sum(n.00,na.rm=T)) %>%
  tidyr::pivot_longer(cols = 4:7)

library(ggplot2)

ggplot(data=sock_pltdat,aes(y=value,x=defin_det_yr)) + 
  geom_bar(stat="identity")+
  geom_hline(yintercept=10,color="red",linewidth=2)


mcn_det_dh <- mcn_det_dh %>% mutate(defin_det_yr=as.numeric(defin_det_yr))
ggplot(data=mcn_det_dh,
       aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("MCN det") +
  theme(legend.position="none")



###################################### #
# 
###################################### #



mcn_dh_tab <- mcn_obs_rel_forDH %>% 
  
  filter(month ==1) %>% # only considering certain locations for model
  
  filter(loc_consid %in% mcn_loc_considered) %>% # only considering certain locations for model
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
            AVIAN_recov=any(AVIAN_recov),
            within=any(within),
            event=unique(event)) %>%
  # mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
  #        DH_label=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label_ant)) 
  mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
         DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> Estuary"),DH_label_ant)) 


mcn_dh_tab2 <- mcn_dh_tab %>% 
  ungroup() %>%
  select(-grp_code,-code)


mcn_dh_tab2_w <- mcn_dh_tab2 %>% 
  filter() %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>% 
  summarize(value=length(unique(tagid))) %>%
  mutate(DH_label=factor(DH_label,levels=c('MCN -> BON -> Estuary','MCN -> BON','MCN -> Estuary','MCN'))) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>%
  tidyr::pivot_wider(names_from=DH_label)

mcn_dh_tab3_w <- mcn_dh_tab2_w %>% 
  rename(n.11='MCN -> BON -> Estuary',
         n.10='MCN -> BON',
         n.01='MCN -> Estuary',
         n.00='MCN')

# # mcn_det_dh <- mcn_dh_tab3_w %>% filter(dat_grp=="mcn_det")
# mcn_det_dh %>% filter(esutype=="SR_Sock")


mcn_det_dh %>% filter(esutype=="SR_Sock") %>%   
  group_by(dat_grp,esutype,defin_det_yr) %>% 
  summarize(n.11=sum(n.11,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.00=sum(n.00,na.rm=T))

ggplot(data=mcn_det_dh,
       aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("MCN det") +
  theme(legend.position="none")





