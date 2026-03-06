# UNTITLED SCRAPS


########## #
# CHUNK 1
########## #

# adding definitive detection times to data
obs_rel_grps2 <- obs_rel_grps %>% 
  left_join(defin_detDF %>% 
              select(dat_grp,esutype,tagid,code,mintime,defin_det_time)) %>%
  arrange(dat_grp,esutype,tagid,mintime) 

# defin_detDF$code2 <- paste(defin_detDF$code2,defin_detDF$mintime)
obs_rel_grps2$defin_det_time_grp <- defin_detDF$defin_det_time[match(obs_rel_grps2$code,defin_detDF$code)]
obs_rel_grps2$defin_det_yr <- defin_detDF$defin_det_yr[match(obs_rel_grps2$code,defin_detDF$code)]
obs_rel_grps2$det_init <- obs_rel_grps2$defin_det_time_grp==obs_rel_grps2$defin_det_time
obs_rel_grps2$det_init[is.na(obs_rel_grps2$det_init)]=FALSE

# difference in "days" between the detection time and the definitive detection time identified for that group
bt=proc.time()
obs_rel_grps2$det_int_days <- difftime(obs_rel_grps2$mintime,obs_rel_grps2$defin_det_time_grp,units = "days")
proc.time()-bt


# code-level summary
tagsum2 <- obs_rel_grps2 %>%
  group_by(dat_grp,esutype,reartype,tagid,code) %>%
  summarize(n_defins=sum(det_init),
            n_dets=length(det_init))

table(tagsum2$n_defins)


subb <- subset(obs_rel_grps2,code %in% tagsum2[tagsum2$n_defins==0,]$code)

subb_all_good <- subb %>% 
  group_by(stage,dat_grp,esutype,reartype) %>%
  summarize(
    n_codes=length(unique(code)),
    n_defins=sum(det_init),
    LGRRRR=sum(obssite=="LGRRRR" & stage=="J"),
    MCJ=sum(obssite=="MCJ"),
    LGR=sum(obssite %in% c("GRS","GRJ")),
    n_dets=length(det_init)) #

subb_all_good %>% filter(stage=="J")
subb_all_good %>% filter(stage!="J")


table(obs_rel_grps2$dat_grp,obs_rel_grps2$esutype)
table(subb$dat_grp,subb$esutype)


# tagid-level summary
tagsum3 <- obs_rel_grps2 %>%
  group_by(tagid) %>%
  summarize(n_defins=sum(det_init),
            n_dets=length(det_init))

# some tags are used more than once
barplot(table(tagsum3$n_defins))


tgs_none <- tagsum3$tagid[tagsum3$n_defins==0]

obs_rel_grps2$init_det <- obs_rel_grps2$mintime==obs_rel_grps2$defin_det_time
table(is.na(obs_rel_grps2$init_det))

# table(obs_rel_grps2[is.na(obs_rel_grps2$init_det),]$tagid,)
obs_rel_grps2$init_det[is.na(obs_rel_grps2$init_det)]=FALSE
table(obs_rel_grps2$init_det)
# head(LGR_DH_matchDF)


# breakdown of tags present in each dat_grp
table(table(tagDF_rel_grps$tagid))
#       1       2       3 
# 1075116  975316  149009 
# 1110146 1083828  151194
################################################### #
# Differences in days from definitive detection 
################################################### #

all_difs <- as.numeric(obs_rel_grps2$det_int_days)
table(is.na(all_difs))

par(mfrow=c(2,2))
hist(all_difs)
# detected in the last 100 days or close to a year previous
hist(all_difs[all_difs<0])
text(x=-700,y=200000,label="DDE probably missclassified as a 'J'",col=2)
# detected less than 100 days later or several hundred days later
hist(all_difs[all_difs>0])
text(x=750,y=500000,label="Probable adult detections",col=2)
# multimodal detections years later
hist(all_difs[all_difs>100])
text(x=750,y=500000,label="Probable adult detections",col=2)

################################################### #



obs_rel_grps2$unassagn <- is.na(as.numeric(obs_rel_grps2$det_int_days))
table(obs_rel_grps2$unassagn)
obs_rel_grps_unasg <- obs_rel_grps2[obs_rel_grps2$unassagn,]

table(obs_rel_grps2$unassagn)
head(obs_rel_grps2)

obs_rel_grps3 <- subset(obs_rel_grps2,!unassagn & det_int_days >= 0 )

# now 3731384, was 3472775 # total codes 
length(unique(obs_rel_grps2$code))
# now 3730094, was 3471774 # total codes with a definitive detection assignmet
length(unique(obs_rel_grps2$code[obs_rel_grps2$det_init]))


# before removing negative values relative to the definitive detection
saveRDS(obs_rel_grps2,"comp_files/obs_rel_grps_2_9825.rds")

# after removing negative values relative to the definitive detection
saveRDS(obs_rel_grps3,"comp_files/obs_rel_grps_3_9825.rds")
saveRDS(tagDF_rel_grps,"comp_files/tagDF_rel_grps_9825.rds")

########## #
# CHUNK 2
########## #
library(dplyr)
library(ggplot2)

# mcn_obs_rel_forDH <- readRDS("comp_files/mcn_obs_rel_forDH")
lgr_obs_rel_forDH <- readRDS("comp_files/old_rec/lgr_obs_rel_forDH.rds")
bin_tab_ls_combDFwYR <- readRDS("comp_files/old_rec/bin_tab_ls_combDFwYR_9825_wPD568.rds")

# lgr_obs_rel_forDH

head(data.frame(obs_rel_grps8))

head(bin_tab_ls_combDFwYR)
# obs_rel_grps6 <- readRDS("comp_files/obs_rel_grps6.rds")
obs_rel_grps8 <- readRDS("comp_files/obs_rel_grps8.rds")
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

lgr_dh_tab$DH_label1 <- gsub(lgr_dh_tab$DH_label,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
lgr_dh_tab$DH_label2 <- gsub(lgr_dh_tab$DH_label1,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
lgr_dh_tab$DH_label3 <- gsub(lgr_dh_tab$DH_label2,pattern="-> BONplus -> BONplus",replacement="-> BONplus")
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
  select(-DH_label1,-DH_label2,-DH_label3,-DH_label4,-DH_label5,-DH_label6,-grp_code,-code)


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



############ #
# CHUNK 3
############ #

library(dplyr)

obs_rel_grps8 <- readRDS("comp_files/obs_rel_grps8.rds")
obs_rel_grps9 <- readRDS("comp_files/obs_rel_grps9.rds")

tagDF_rel_grps <- readRDS("comp_files/tags_and_obs_comb_ls9825.rds")$"tags_comb"  

lgr_obs_rel_wdups <- obs_rel_grps9 %>%
  filter(dat_grp %in% c("lgr_pooled","lgr_det")) %>%
  mutate(dup_obs=duplicated(code2))

lgr_obs_rel_forDH <- lgr_obs_rel_wdups %>% 
  filter(!dup_obs) %>% 
  left_join(tagDF_rel_grps %>% select(tagid,AVIAN_recov)) %>%
  left_join(obs_rel_grps8 %>% select(dat_grp,esutype,reartype,tagid,day,days3.5,week1,weeks2,month)) %>%
  mutate(loc_consid=ifelse(prim_loc_cat %in% c("BON","JDA","Estuary"),"BONplus",prim_loc_cat))

lgr_loc_considered <- c("LGR","MCN","BONplus")

lgr_dh_tab <- lgr_obs_rel_forDH %>% 
  filter(within) %>%
  filter(loc_consid %in% lgr_loc_considered) %>% # only considering certain locations for model
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(DH_label_ant=paste(loc_consid,collapse=" -> "),
            AVIAN_recov=any(AVIAN_recov),
            within=any(within),
            event=unique(event)) %>%
  mutate(no_estu_end=substr(x=DH_label_ant,(nchar(DH_label_ant)-9),nchar(DH_label_ant))!="-> BONplus",
         DH_label=ifelse(no_estu_end & AVIAN_recov,paste0(DH_label_ant," -> BONplus"),DH_label_ant)) 

DH_label_LGR_ls <- list()

################################################################### #
# recursive relabeling to form 3-occasion groupings
################################################################### #
DH_label_LGR_ls[[1]] <- sort(table(lgr_dh_tab$DH_label),decreasing=T)

lgr_dh_tab$DH_label1 <- gsub(lgr_dh_tab$DH_label,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[2]] <- sort(table(lgr_dh_tab$DH_label1),decreasing=T)
lgr_dh_tab$DH_label2 <- gsub(lgr_dh_tab$DH_label1,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[3]] <- sort(table(lgr_dh_tab$DH_label2),decreasing=T)
lgr_dh_tab$DH_label3 <- gsub(lgr_dh_tab$DH_label2,pattern="-> BONplus -> BONplus",replacement="-> BONplus"); DH_label_LGR_ls[[4]] <- sort(table(lgr_dh_tab$DH_label3),decreasing=T)
lgr_dh_tab$DH_label4 <- gsub(lgr_dh_tab$DH_label3,pattern="LGR -> BONplus -> MCN -> BONplus",replacement="LGR -> BONplus"); DH_label_LGR_ls[[5]] <- sort(table(lgr_dh_tab$DH_label4),decreasing=T)
lgr_dh_tab$DH_label5 <- gsub(lgr_dh_tab$DH_label4,pattern="LGR -> BONplus -> MCN",replacement="LGR -> BONplus"); DH_label_LGR_ls[[6]] <- sort(table(lgr_dh_tab$DH_label5),decreasing=T)
lgr_dh_tab$DH_label6 <-  gsub(lgr_dh_tab$DH_label5,pattern="BONplus",replacement="BON"); DH_label_LGR_ls[[7]] <- sort(table(lgr_dh_tab$DH_label6),decreasing=T)
lgr_dh_tab$DH_label7 <-  gsub(lgr_dh_tab$DH_label6,pattern="LGR -> LGR",replacement="LGR"); DH_label_LGR_ls[[8]] <- sort(table(lgr_dh_tab$DH_label7),decreasing=T)
lgr_dh_tab$DH_label_orig <- lgr_dh_tab$DH_label
lgr_dh_tab$DH_label <- lgr_dh_tab$DH_label7

DH_label_LGR_ls


lgr_dh_tab2 <- lgr_dh_tab %>% 
  ungroup() %>%
  select(-DH_label1,-DH_label2,-DH_label3,-DH_label4,-DH_label5,-DH_label6,-grp_code,-code) %>%
  mutate(n.11='LGR -> MCN -> BON',
         n.10='LGR -> MCN',
         n.01='LGR -> BON',
         n.00='LGR')


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

# subsetting MCN det data
mcn_obs_rel_wdups <- obs_rel_grps9 %>% 
  filter(dat_grp=="mcn_det" & !(prim_loc_cat %in% c("LGR","LMN","LGS","ICH"))) %>%
  mutate(dup_obs=duplicated(code2))

mcn_obs_rel_forDH <-  mcn_obs_rel_wdups %>%
  filter(!dup_obs) %>% 
  left_join(tagDF_rel_grps %>% select(tagid,AVIAN_recov)) %>%
  left_join(obs_rel_grps8 %>% select(dat_grp,esutype,reartype,tagid,day,days3.5,week1,weeks2,month))


