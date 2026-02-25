library(dplyr)
obs_rel_grps6 <- readRDS("temp/obs_rel_grps6.rds")
nrow(obs_rel_grps6)

obs_rel_grps6$defin_det_time_grp # tag-specific definitive detection time
obs_rel_grps6$grp_code #dat_grp esutype reartype defin_det_yr
obs_rel_grps6$code # dat_grp esutype reartype tagid


table(obs_rel_grps6$dat_grp)

subb_6_dh <-  obs_rel_grps6 %>% 
  filter(esutype=="SR_Ch1" & dat_grp=="lgr_det") %>% 
  group_by(reartype,tagid,defin_det_yr) %>%
  summarize(DH_label=paste(prim_loc_cat,collapse=" -> "))





# dh_tab <- obs_rel_grps9 %>% 
#   group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code) %>%
#   summarize(DH_label=paste(prim_loc_cat,collapse=" -> "))








# 
# obs_rel_grps7 <- readRDS("temp/obs_rel_grps7.rds")
# 
# > nrow(obs_rel_grps2 )
# [1] 6894635
# > length(unique(obs_rel_grps2$tagid))
# [1] 2345168
# nrow(obs_rel_grps7)/6894635 40% of the detections
length(unique(obs_rel_grps7$tagid))
# length(unique(obs_rel_grps7$tagid))/2345168 68% of the tags


str(obs_rel_grps7)
View(obs_rel_grps7)


nrow(obs_rel_grps2)
length(unique(obs_rel_grps2$tagid))

# nrow(obs_rel_grps3)
# nrow(obs_rel_grps3)/nrow(obs_rel_grps2)
# length(unique(obs_rel_grps3$tagid))
# length(unique(obs_rel_grps3$tagid))/length(unique(obs_rel_grps2$tagid))
# 
# 
# nrow(obs_rel_grps4)
# nrow(obs_rel_grps4)/nrow(obs_rel_grps2)
# length(unique(obs_rel_grps4$tagid))
# length(unique(obs_rel_grps4$tagid))/length(unique(obs_rel_grps2$tagid))
# 
# nrow(obs_rel_grps5)
# nrow(obs_rel_grps5)/nrow(obs_rel_grps2)
# length(unique(obs_rel_grps5$tagid))
# length(unique(obs_rel_grps5$tagid))/length(unique(obs_rel_grps2$tagid))
# 
# 
# nrow(obs_rel_grps6)
# nrow(obs_rel_grps6)/nrow(obs_rel_grps2)
# length(unique(obs_rel_grps6$tagid))
# length(unique(obs_rel_grps6$tagid))/length(unique(obs_rel_grps2$tagid))


