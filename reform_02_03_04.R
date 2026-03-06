
# I should be able to look up all the tagged fish that contribute to an estimate in a row count



raw_DF <- readRDS("comp_files/DART_ALL_SR_ESU_DPS_rec_ls.rds")$"raw_DF"
tagDF <- readRDS("comp_files/DART_ALL_SR_ESU_DPS_rec_ls.rds")$"tagDF"


# anything besides
raw_DF <- DART_ALL_SR_ESU_DPS_rec_ls$"raw_DF"
# head(tagDF$dets)
# head(tagDF$trans_stat)



int_recov_sites_ls <- readRDS("comp_files/int_recov_sites_ls.rds") # from 01b (formerly 02c)


system.time(obs_comb_raw <- readRDS("temp/tags_and_obs_comb_raw_ls9825.rds")$"obs_comb")
system.time(tags_comb_raw <- readRDS("temp/tags_and_obs_comb_raw_ls9825.rds")$"tags_comb")


raw_TWX_DF <- readRDS("temp/data_9825_TWX/raw_TWX_DF_9825.rds")

# all(raw_TWX_DF$tagid %in% c(tags_comb_raw$tagid))
# head(tags_comb_raw)
# all(raw_TWX_DF$tagid %in% c(obs_comb_raw$tagid))
# head(raw_TWX_DF)
obs_comb_rawNOTWX <- obs_comb_raw


############################################### #
# ADDING TWX DETAILS  as columns in OBS data
############################################### #

library(dplyr)
bt=proc.time()
obs_comb_raw <- obs_comb_raw %>% dplyr::left_join(raw_TWX_DF %>% dplyr::select(tagid,obssite,obsdetail,mintime))
proc.time()-bt

TWX_tags <- unique(obs_comb_raw %>% filter(!is.na(obs_comb_raw$obsdetail)) %>% pull(tagid))
obs_comb_TWX <- obs_comb_raw %>% filter(tagid %in% TWX_tags)

# TOWED
table(obs_comb_raw$obsdetail,obs_comb_raw$migryr)

# relevant mosly b/c TOWED FLEXIBLE ANTENNA SYSTEM detections are broken out
head(data.frame(table(obs_comb_raw$obssite)[order(names(table(obs_comb_raw$obssite)))]))


############################################### #
# ADDING AVIAN DETAILS  as columns in OBS data
############################################### #

AV_recov_relyr9625_DF <- readRDS("comp_files/AV_recov_relyr9625.rds")

# c(12468095,248618)/nrow(tags_comb_raw) 
# about 2% of the tags in the raw release data set have Avian mort recovery
table(tags_comb_raw$tagid %in% AV_recov_relyr9625_DF$tagid)

# there are no duplicate recoveries
# table(duplicated(AV_recov_relyr9625_DF$tagid))

tags_comb_raw <- tags_comb_raw %>% left_join(AV_recov_relyr9625_DF %>% rename(recovsite=obssite) %>% select(tagid,recovsite,reldates),by="tagid")
tags_comb_raw <- tags_comb_raw %>% mutate(hist_type_mod=ifelse(hist_type=="release_only" & !is.na(recovsite),"avian_recovery",hist_type))

table(tags_comb_raw$hist_type_mod)
barplot(table(tags_comb_raw$hist_type_mod))
# 66025/(66025+5527075) # slightly more than 1% of fish are ever seen again at avian colonies

# more that 1/2 of released ESU/DPS fish that are released are not detected as juveniles

head(tags_comb_raw)
head(tags_comb_raw[!is.na(tags_comb_raw$recovsite),])


# SCRAP 1
# 
# # loading in comb tags and obs
# 
# obs_comb <- readRDS("temp/tags_and_obs_comb_ls9825.rds")$"obs_comb"
# tags_comb <- readRDS("temp/tags_and_obs_comb_ls9825.rds")$"tags_comb"
# 
# table(obs_comb$relbasin)
# 
# # source("02c_load_spat_dat.R")
# head(tags_comb_raw)
# 
# table(obs_comb$event)



# SCRAP 2
# obs_rel_grps %>% 
#   filter(esutype=="SR_Ch1" & 
#            year==2013 & #
#            prim_loc_cat =="LGR" & 
#            lgr_det) %>% 
#   group_by(reartype) %>% 
#   summarize(ntags=length(unique(tagid)))
# 
# 
# table(obs_rel_grps$year,obs_rel_grps$relsite =="LGRRRR")
# 
# obs_rel_grps %>% 
#   filter(esutype=="SR_Ch1" & 
#            year==2013 & #
#            prim_loc_cat =="LGR" & 
#            # lgr_det &
#            relsite =="LGRRRR") %>% 
#   group_by(reartype) %>% 
#   summarize(ntags=length(unique(tagid)))
# 
# 
# 8769+5740
