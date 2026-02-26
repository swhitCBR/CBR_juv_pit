library(dplyr)
obs_rel_grps6 <- readRDS("comp_files/obs_rel_grps6.rds")
# obs_rel_grps7 <- readRDS("comp_files/obs_rel_grps7.rds")

# nrow(obs_rel_grps6)
bin_tab_ls_combDF <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")


# obs_rel_grps9 <- obs_rel_grps6 %>%
#   # filter(within) %>%
#   mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"))


obs_rel_grps9 <-  readRDS("comp_files/obs_rel_grps9.rds")

# obs_rel_grps10 <- obs_rel_grps6 %>%
#   filter(within) %>%
#   mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"))


head(obs_rel_grps9$code2)
#code that is duplicaed condains: dat_grp,esutype reartype tag and location

# subsetting MCN det data
mcn_obs_rel <- obs_rel_grps9 %>% filter(dat_grp=="mcn_det" & prim_loc_cat %in% c("MCN","BON","JDA","Estuary"))

mcn_dup_tagid <- unique(mcn_obs_rel$tagid[duplicated(mcn_obs_rel$code2)])
length(mcn_dup_tagid) # only 157 tags

# BON duplicated
data.frame(mcn_obs_rel %>% filter(tagid %in% mcn_dup_tagid[1]) %>% 
             select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime))



lgr_obs_rel_wdups <- obs_rel_grps9 %>%
  filter(dat_grp %in% c("lgr_pooled","lgr_det"))# %>% 
  # mutate(dup_code=duplicated(code2))

# lgr_obs_rel_wdups %>% group_by(dat_grp) %% summarize(ndups=sum(dup_code))

lgr_dup_tagid <- unique(lgr_obs_rel_wdups$tagid[duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid) # only 157 tags

lgr_dup_tagid_det <- unique(lgr_obs_rel_wdups$tagid[ lgr_obs_rel_wdups$dat_grp=="lgr_det" & duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid_det) # only 116 tags
lgr_dup_tagid_pool <- unique(lgr_obs_rel_wdups$tagid[ lgr_obs_rel_wdups$dat_grp=="lgr_pooled" & duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid_pool) # only 144 tags

lgr_dup_tagid_pool_only <- lgr_dup_tagid_pool[!lgr_dup_tagid_pool %in% lgr_dup_tagid_det]



data.frame(lgr_obs_rel_wdups %>% filter( tagid %in% lgr_dup_tagid_det[1]) %>% 
             select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime))




data.frame(lgr_obs_rel_wdups %>% filter(dat_grp =="lgr_det" & tagid %in% lgr_dup_tagid_det[1]) %>% 
             select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime))

data.frame(lgr_obs_rel_wdups %>% filter(dat_grp =="lgr_pooled" & tagid %in% lgr_dup_tagid_pool[1]) %>% 
             select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime))

data.frame(lgr_obs_rel_wdups %>% filter(dat_grp =="lgr_pooled" & tagid %in% lgr_dup_tagid_pool_only[1]) %>% 
             select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime))


data.frame(lgr_obs_rel_wdups %>% filter(tagid %in% lgr_dup_tagid_pool_only[1]))


View(lgr_obs_rel_wdups %>% filter(dat_grp =="lgr_pooled" & event=="virt_detection(>0)"))





# data.frame(obs_rel_grps9 %>% filter(tagid %in% dup_tagid[1]))
data.frame(obs_rel_grps9 %>% filter(tagid %in% dup_tagid[1]) %>% 
             select(esutype,reartype,prim_loc_cat,event,obssite,mintime))
obs_rel_grps9b <- obs_rel_grps9[!duplicated(obs_rel_grps9$code2),]


data.frame(obs_rel_grps9b %>% filter(tagid %in% dup_tagid[1]) %>% 
             select(esutype,reartype,prim_loc_cat,event,obssite,mintime))

table(obs_rel_grps9b$obssite)
table(obs_rel_grps9b$prim_loc_cat)
table(obs_rel_grps9b$obssite,obs_rel_grps9b$prim_loc_cat)
table(is.na(obs_rel_grps9b$obssite))

mcn_obs_rel <- obs_rel_grps9 <- filter(dat_grp=="mcn_det" %>% prim_loc_cat <- c("MCN","BON","JDA","Estuary"))




data.frame(obs_rel_grps9 %>% filter(dat_grp=="lgr_pooled" & tagid %in% dup_tagid[2]) %>% 
             select(esutype,reartype,prim_loc_cat,obssite,mintime))
# 
# tagDF_rel_grps %>% filter(tagid %in% dup_tagid ) %>% 
#   select(esutype,reartype,tagid) %>% 
#   group_by(esutype,reartype) %>% 
#   summarize(ntags=length(unique(tagid))) 

# what's up with the duplicated obs_rel
obs_rel_grps9b <- obs_rel_grps9[!duplicated(obs_rel_grps9$code2),]
