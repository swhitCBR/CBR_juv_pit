
library(dplyr)

obs_rel_grps9 <- readRDS("comp_files/obs_rel_grps9.rds")

# subsetting MCN det data
mcn_obs_rel <- obs_rel_grps9 %>% 
  filter(dat_grp=="mcn_det" & prim_loc_cat %in% c("MCN","BON","JDA","Estuary")) %>%
  mutate(dup_obs=duplicated(code2))


mcn_dup_tagid <- unique(mcn_obs_rel$tagid[duplicated(mcn_obs_rel$code2)])
length(mcn_dup_tagid) # only 157 tags

# BON duplicated
mcn_dets_wdupsDF <- mcn_obs_rel %>% filter(tagid %in% mcn_dup_tagid)

# ###################################################################################### #
# PRINTED OUT AND REVIEWED THE 16 tags in question
# lapply(mcn_dup_tagid,function(x){
#   mcn_dets_wdupsDF %>%
#     filter(tagid==x) %>%
#     select(dat_grp,esutype,reartype,relsite,prim_loc_cat,event,obssite,mintime)})
# 
# mcn_dets_wdupsDF %>% 
#   filter(tagid %in% mcn_dup_tagid[1]) %>% 
#   select(dat_grp,esutype,reartype,prim_loc_cat,event,obssite,mintime)
# 
# mcn_obs_rel %>% filter(!dup_obs)
# ###################################################################################### #

lgr_obs_rel_wdups <- obs_rel_grps9 %>%
  filter(dat_grp %in% c("lgr_pooled","lgr_det")) %>% 
  mutate(dup_obs=duplicated(code2))

lgr_dup_tagid <- unique(lgr_obs_rel_wdups$tagid[duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid) # only 175 tags

lgr_dup_tagid_det <- unique(lgr_obs_rel_wdups$tagid[ lgr_obs_rel_wdups$dat_grp=="lgr_det" & duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid_det) # only 116 tags
lgr_dup_tagid_pool <- unique(lgr_obs_rel_wdups$tagid[ lgr_obs_rel_wdups$dat_grp=="lgr_pooled" & duplicated(lgr_obs_rel_wdups$code2)])
length(lgr_dup_tagid_pool) # only 144 tags

lgr_dup_tagid_pool_only <- lgr_dup_tagid_pool[!lgr_dup_tagid_pool %in% lgr_dup_tagid_det]
length(lgr_dup_tagid_pool_only)

lgr_dets_wdupsDF <- lgr_obs_rel_wdups %>% 
  filter(tagid %in% lgr_dup_tagid) %>% 
  mutate(td_dup=difftime(mintime,defin_det_time_grp,units="days"))

lgr_dets_wdupsDF %>% select(dat_grp,esutype,reartype,event,prim_loc_cat,event,obssite,mintime,dup_obs,td_dup)
lgr_dets_wdupsDF %>% filter(dup_obs) %>% group_by(dat_grp,prim_loc_cat) %>% summarize(ntags=length(unique(tagid))) 

lgr_dets_wdupsDF %>% filter(dup_obs) %>% mutate(td=difftime(mintime,defin_det_time_grp,units="days")) %>%
  select(dat_grp,esutype,reartype,event,prim_loc_cat,event,obssite,mintime,dup_obs,td)

