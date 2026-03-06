library(dplyr)
library(ggplot2)

# mcn_obs_rel_forDH <- readRDS("comp_files/mcn_obs_rel_forDH")
lgr_obs_rel_forDH <- readRDS("comp_files/lgr_obs_rel_forDH.rds")
bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

head(bin_tab_ls_combDFwYR)
# obs_rel_grps6 <- readRDS("comp_files/obs_rel_grps6.rds")
# obs_rel_grps8 <- readRDS("comp_files/obs_rel_grps8.rds")
lgr_obs_rel_forDH$loc_consid <- ifelse(lgr_obs_rel_forDH$prim_loc_cat %in% c("BON","JDA","Estuary"),"BONplus",lgr_obs_rel_forDH$prim_loc_cat)

lgr_loc_considered <- c("LGR","MCN","BONplus")

table(lgr_obs_rel_forDH$prim_loc_cat)
table(lgr_obs_rel_forDH$loc_consid)






# # CONVERT TO ANTENNA LEVEL VERSION
# lgr_dh_tabANT <- lgr_obs_rel_forDH %>% 
#   filter(loc_consid %in% lgr_loc_considered) %>% # only considering certain locations for model
#   group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
#   summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
#             AVIAN_recov=any(AVIAN_recov),
#             within=any(within)) %>%
#   mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
#          DH_label=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label_ant))
#   ### mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> BONplus",
#   ####        DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> BONplus"),DH_label_ant)) 


lgr_dh_tab <- lgr_obs_rel_forDH %>% 
  filter(within) %>%
  filter(loc_consid %in% lgr_loc_considered) %>% # only considering certain locations for model
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
            AVIAN_recov=any(AVIAN_recov),
            within=any(within),
            event=unique(event)) %>%
  # mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
  #        DH_label=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label_ant)) 
  mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> BONplus",
         DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> BONplus"),DH_label_ant)) 

sort(table(lgr_dh_tab$DH_label),decreasing=T)


lgr_dh_tab$DH_label1 <- gsub(lgr_dh_tab$DH_label,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
lgr_dh_tab$DH_label2 <- gsub(lgr_dh_tab$DH_label1,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
sort(table(lgr_dh_tab$DH_label2),decreasing=T)


lgr_dh_tab$DH_label3 <- gsub(lgr_dh_tab$DH_label2,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
sort(table(lgr_dh_tab$DH_label3),decreasing=T)

# These ones are shortened  (6 tags total)
lgr_dh_tab$DH_label4 <- gsub(lgr_dh_tab$DH_label3,pattern="LGR -> BONplus -> MCN -> BONplus",replacement="LGR -> BONplus")
lgr_dh_tab$DH_label5 <- gsub(lgr_dh_tab$DH_label4,pattern="LGR -> BONplus -> MCN",replacement="LGR -> BONplus")
# sort(table(lgr_dh_tab$DH_label3),decreasing=T)
# sort(table(lgr_dh_tab$DH_label5),decreasing=T)
lgr_dh_tab$DH_label6 <-  gsub(lgr_dh_tab$DH_label5,pattern="BONplus",replacement="BON")
sort(table(lgr_dh_tab$DH_label6),decreasing=T)

lgr_dh_tab$DH_label_orig <- lgr_dh_tab$DH_label
lgr_dh_tab$DH_label <- lgr_dh_tab$DH_label6

lgr_dh_tab2 <- lgr_dh_tab %>% 
  ungroup() %>%
  select(-DH_label1,-DH_label2,-DH_label3,-DH_label4,-DH_label5,-DH_label6,-grp_code,-code) %>%
  mutate(n.11='LGR -> MCN -> BON',
         n.10='LGR -> MCN',
         n.01='LGR -> BON',
         n.00='LGR')

tagDF_rel_grps <- readRDS("comp_files/tags_and_obs_comb_ls9825.rds")$"tags_comb"


# tg_tmp <- lgr_dh_tab2$tagid
# tg_tmp2 <-  unique(tg_tmp[lgr_dh_tab2$dat_grp=="lgr_pooled"])[unique(tg_tmp[lgr_dh_tab2$dat_grp=="lgr_pooled"]) %in% 
#                                                                unique(tg_tmp[lgr_dh_tab2$dat_grp=="lgr_det"])]
# length(tg_tmp2)

######################################### #
# adding release group information
######################################### #

# tagDF_rel_grps$lgr_pooled <- tagDF_rel_grps$tagid %in% 
# tagDF_rel_grps$tagid

lgr_det_tgs <- lgr_dh_tab2 %>% filter(dat_grp=="lgr_det") %>% pull(tagid)
lgr_pooled_tgs <- lgr_dh_tab2 %>% filter(dat_grp=="lgr_pooled") %>% pull(tagid)

tagDF_rel_grps$lgr_det <- tagDF_rel_grps$tagid %in% lgr_det_tgs
tagDF_rel_grps$lgr_pooled <- tagDF_rel_grps$tagid %in% lgr_pooled_tgs

table(paste(tagDF_rel_grps$lgr_det,tagDF_rel_grps$lgr_pooled))
head(tagDF_rel_grps[tagDF_rel_grps$lgr_det & !tagDF_rel_grps$lgr_pooled,])





lgr_dh_tab2$within

# lgr_pool_outsite_det_tgs <- lgr_dh_tab2 %>% 
#   filter(dat_grp=="lgr_pooled" & 
#            tagid %in% lgr_dh_tab2 %>% 
#            filter(dat_grp=="lgr_det") %>% 
#            pull(tagid)) %>%
#   pull(tagid)
# 
# lgr_dh_tab2_unktgDF <- lgr_dh_tab2 %>% 
#   filter(dat_grp=="lgr_det" & 
#            (dat_grp=="lgr_pooled" & 
#               tagid %in% lgr_pool_outsite_det_tgs))

######################################### #




# subset(lgr_dh_tab2,lgr_dh_tab2$dat_grp=="lgr_pooled")

table(lgr_dh_tab2$dat_grp)
# head(lgr_dh_tab2 %>% select()


tagDF_rel_grps$tagid

tagDF_rel_grps
lgr_dh_tab2$tagid 

############################################## #
# individual tags converted to cell counts
############################################## #

lgr_dh_tab2_w <- lgr_dh_tab2 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>% 
  summarize(value=length(unique(tagid))) %>%
  mutate(DH_label=factor(DH_label,levels=c('LGR -> MCN -> BON','LGR -> MCN','LGR -> BON','LGR'))) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,DH_label) %>%
  tidyr::pivot_wider(names_from=DH_label)




lgr_dh_tab3_w <- lgr_dh_tab2_w %>% 
  rename(n.11='LGR -> MCN -> BON',
         n.10='LGR -> MCN',
         n.01='LGR -> BON',
         n.00='LGR')


lgr_dh_tab3_w %>% filter(defin_det_yr==2008 & esutype=="SR_Ch1")


saveRDS(lgr_dh_tab3_w,"comp_files/lgr_dh_tab3_w.rds")

source("C:/repos/repo_simCJS/simCJS/R/per2_surph_ests.R")
source("C:/repos/repo_simCJS/simCJS/R/get_est_tab.R")
# View(cbind(lgr_dh_tab3_w,get_est_tab(df_in = lgr_dh_tab3_w)))

lgr_det_dh <- lgr_dh_tab3_w %>% filter(dat_grp=="lgr_det")
lgr_pool_dh <- lgr_dh_tab3_w %>% filter(dat_grp=="lgr_pooled")
# lgr_det_dh

# View(lgr_det_dh %>% filter(esutype=="SR_Sock"))


# Including sockeye
lgr_det_dh <- lgr_det_dh %>% mutate(defin_det_yr=as.numeric(defin_det_yr))
lgr_det_plt_base_wSOCK <- ggplot(data=lgr_det_dh,
       aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR det") +
  theme(legend.position="none")

lgr_det_plt_base_wSOCK
lgr_det_plt_base_wSOCK + scale_x_continuous(limits=c(2008,2025))




############################# #
# LGR_DET AND NO SOCKEYE
############################# #

lgr_det_dh_noSock <- lgr_det_dh %>% filter(esutype!="SR_Sock") %>% 
  mutate(defin_det_yr=as.numeric(defin_det_yr)) %>% 
  ungroup()
lgr_det_plt_base <- ggplot(data=lgr_det_dh_noSock,
                           aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR det") +
  theme(legend.position="none")

lgr_det_plt_base
lgr_det_plt_base + scale_x_continuous(limits=c(2008,2026))
lgr_det_plt_base + scale_x_continuous(limits=c(2015.5,2025.5),breaks = 2016:2025)




############################# #
# POOLED AND NO SOCKEYE
############################# #


lgr_pool_dh_noSock <- lgr_pool_dh %>% filter(esutype!="SR_Sock") %>% 
  mutate(defin_det_yr=as.numeric(defin_det_yr)) %>% 
  ungroup()
lgr_pool_plt_base <- ggplot(data=lgr_pool_dh_noSock,
                           aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR pooled") +
  theme(legend.position="none")

lgr_pool_plt_base
lgr_pool_plt_base + scale_x_continuous(limits=c(2008,2026))
lgr_pool_plt_base + scale_x_continuous(limits=c(2015.5,2025.5),breaks = 2016:2025)



RT_comb_lgr_pool_dh<- lgr_pool_dh %>% 
  group_by(dat_grp,esutype,defin_det_yr) %>%
  summarize(n.11=sum(n.11,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.00=sum(n.00,na.rm=T)) %>% 
    mutate(defin_det_yr=as.numeric(defin_det_yr))


RT_comb_lgr_pool_dh_noSock <- RT_comb_lgr_pool_dh %>% filter(esutype!="SR_Sock")


# RT_comb_lgr_pool_dh_noSock <- RT_comb_lgr_pool_dh_noSock %>% 
#   mutate(defin_det_yr=as.numeric(defin_det_yr)) %>% 
#   ungroup()
RT_comb_lgr_pool_plt_base <- ggplot(data=RT_comb_lgr_pool_dh,
                            aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  facet_wrap(~esutype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR pooled") +
  theme(legend.position="none")

RT_comb_lgr_pool_plt_base


RT_comb_lgr_pool_plt_base <- ggplot(data=RT_comb_lgr_pool_dh_noSock,
                                    aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  facet_wrap(~esutype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR pooled") +
  theme(legend.position="none")

RT_comb_lgr_pool_plt_base







# lgr_det_dh <- lgr_det_dh %>% filter(esutype!="SR_Sock") %>% mutate(defin_det_yr=as.numeric(defin_det_yr))

lgr_det_plt_base <- ggplot(data=lgr_det_dh,
                           aes(y=n.11,x=defin_det_yr,fill=n.11>=10)) + 
  geom_bar(stat="identity",color="black") +
  # facet_wrap(~esutype,scales="free_y") +
  facet_grid(esutype~reartype,scales="free_y") +
  geom_hline(yintercept=10,color="darkred",linewidth=0.75)+
  ggtitle("LGR det") +
  theme(legend.position="none")

lgr_det_plt_base + scale_x_continuous(limits=c(2016,2025))


# lgr_dh_tab3_w
# 

sort(table(lgr_dh_tab$DH_label4),decreasing=T)

sort(table(lgr_dh_tab$DH_label1),decreasing=T)
sort(table(lgr_dh_tab$DH_label2),decreasing=T)


lgr_dh_tab$DH_label <- gsub(lgr_dh_tab$DH_label,"-> BONplus -> BONplus","-> BONplus")
# lgr_dh_tab$DH_label <- gsub(lgr_dh_tab$DH_label,"-> BONplus -> BONplus","-> BONplus")
sort(table(lgr_dh_tab$DH_label),decreasing=T)
sort(table(gsub(lgr_dh_tab$DH_label,pattern="-> BONplus -> BONplus",replacement="-> BONplus")),decreasing=T)



# LGR -> BONplus -> MCN -> BONplus
# LGR -> BONplus -> MCN 


head(data.frame(subset(lgr_dh_tab,DH_label==" -> BONplus")))

lgr_dh_tab

# table(gsub(lgr_dh_tab$DH_label,"-> BONplus -> BONplus","-> BONplus"))
# lgr_dh_tab 
head(bin_tab_ls_combDFwYR)



# lgr_obs_rel_forDH$
# lgr_dh_tab <- lgr_dh_tab %>% mutate(DH_label_mod=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label))
  # mutate(avian_rec=tagid %in% c(rel_recov_tgs))

source("R/LGR_DH_match & MCN_DH_match.R")
# head(lgr_dh_tab)
sort(table(lgr_dh_tab$DH_label),decreasing=T)
# table(dh_tab)

# lgr_loc_considered <- c("LGR","MCN","BONplus")
# lgr_dh_tab$
# lgr_dh_tab$loc_consid <- ifelse(lgr_dh_tab$prim_loc_cat %in% c("Estuary"),"BONplus",lgr_dh_tab$prim_loc_cat)
# lgr_obs_rel_forDH <- 
# 
# lgr_dh_tab <- lgr_dh_tab %>% 
#   filter(!obssite %in% c(LGS,LMN,ICH)) %>%
#   group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
#   summarize(DH_label_ant=paste(prim_loc_cat,collapse=" -> "),
#             AVIAN_recov=any(AVIAN_recov),
#             within=any(within)) %>%
#   mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> Estuary",
#          DH_label=ifelse(no_estu_end & AVIAN_recov," -> Estuary",DH_label_ant)) 




