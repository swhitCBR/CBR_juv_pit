library(foreign)
library(dplyr)
library(reshape2)


bt_all <- proc.time()

#### Loading and combining raw obs and tag files ####
in_dir="comp_files/data_9825_comp"
if(!dir.exists(in_dir)){dir.create(in_dir,recursive = T)}

# separates obs and tags within workspace
ESU_nms=c("SR_Ch1","SR_Sthd","SR_Sock")

raw_DF <- readRDS("comp_files/DART_ALL_SR_ESU_DPS_rec_ls.rds")$"raw_DF"
tagDF <- readRDS("comp_files/DART_ALL_SR_ESU_DPS_rec_ls.rds")$"tagDF"

obs_comb_raw=raw_DF
tags_comb_raw <- data.frame(obs_comb_raw[match(tagDF$tagid,obs_comb_raw$tagid),c("esutype","event","species","run")],tagDF)
names(tags_comb_raw)[2] <- "hist_type"

rm(raw_DF)
rm(tagDF)

# doubted this would work as is
# raw_TWX_DF <- readRDS("temp/data_9823_comp_SI_test_TWX/raw_TWX_DF.RDS")
prim_obssiteDF <-  readRDS("comp_files/int_recov_sites_ls.rds")$"prim_obssiteDF"

############################ #
# ADDING TWX DETAILS  
############################ #

raw_TWX_DF <- readRDS("temp/data_9825_TWX/raw_TWX_DF_9825.rds")

# all(raw_TWX_DF$tagid %in% c(tags_comb_raw$tagid))
# all(raw_TWX_DF$tagid %in% c(obs_comb_raw$tagid))
# obs_comb_rawNOTWX <- obs_comb_raw

bt=proc.time()
obs_comb_raw <- obs_comb_raw %>% left_join(raw_TWX_DF %>% select(tagid,obssite,obsdetail,mintime))
proc.time()-bt

# TWX_tags <- unique(obs_comb_raw %>% filter(!is.na(obs_comb_raw$obsdetail)) %>% pull(tagid))
# obs_comb_TWX <- obs_comb_raw %>% filter(tagid %in% TWX_tags)
# table(obs_comb_raw$obsdetail,obs_comb_raw$migryr)
# table(obs_comb_TWX$obsdetail,obs_comb_TWX$migryr)
# data.frame(table(obs_comb_raw$obssite)[order(names(table(obs_comb_raw$obssite)))])

######################################################################################## #
#### creating tagid lists Adding TRUE/FALSE columns to tag data for filtering ####
######################################################################################## #

# data.frame(table(obs_comb_raw$obssite))

codes_non_lgr_intradam_codes <-  readRDS("comp_files/int_recov_sites_ls.rds")$"codes_non_lgr_intradam_codes"


# tag groupings
tags_w_trans_lab <- unique(obs_comb_raw$tagid[obs_comb_raw$trans_status!=""])
tags_non_lgr_intradam_tags <- tags_comb_raw[tags_comb_raw$relsite %in% codes_non_lgr_intradam_codes,]$tagid
tags_off_lgr_rel <- unique(tags_comb_raw$tagid[tags_comb_raw$relsite=="LGRRRR"])

tags_comb_raw$trans_statTF <- tags_comb_raw$tagid %in% tags_w_trans_lab
tags_comb_raw$nonLGR_intra_dam_rel <- tags_comb_raw$tagid %in% tags_non_lgr_intradam_tags
tags_comb_raw$lgr_rel <- tags_comb_raw$tagid %in% tags_off_lgr_rel

# adding column for excluded tags
tags_comb_raw$excluded=(tags_comb_raw$trans_statTF | tags_comb_raw$nonLGR_intra_dam_rel)
obs_comb_raw$excluded=(obs_comb_raw$tagid %in% c(tags_comb_raw$tagid[tags_comb_raw$excluded]))

# classifying observation sites as Primary or secondary and surface vs. nonsurface
obs_comb_raw$obssite_prim <- obs_comb_raw$obssite %in% prim_obssiteDF$obssite
obs_comb_raw$prim_loc_cat <- prim_obssiteDF$loc_cat[match(obs_comb_raw$obssite,prim_obssiteDF$obssite)]
obs_comb_raw$prim_surface <- obs_comb_raw$obssite %in% c("GRS","BCC")
tags_comb_raw$event <- obs_comb_raw$event[match(tags_comb_raw$tagid,obs_comb_raw$tagid)]

tags_comb_raw$rel_year <- lubridate::year(tags_comb_raw$reltime)
tags_comb_raw$at_LGR <- tags_comb_raw$relsite=="LGRRRR"

#### Avian recoveries associated with tags ### #
AV_recov_relyr9625_tg <- readRDS("comp_files/AV_recov_relyr9625.rds")
tags_comb_raw$AVIAN_recov <- tags_comb_raw$tagid %in% AV_recov_relyr9625_tg$tagid

#### Tag detected at a primary location, LGR, or BON
prim_loc_det_tags <- unique(obs_comb_raw$tagid[!is.na(obs_comb_raw$prim_loc_cat)])
LGR_loc_det_tags <- unique(obs_comb_raw$tagid[obs_comb_raw$prim_loc_cat=="LGR"])
MCN_loc_det_tags <- unique(obs_comb_raw$tagid[obs_comb_raw$prim_loc_cat=="MCN"])

tags_comb_raw$prim_loc_det <- tags_comb_raw$tagid %in% prim_loc_det_tags
tags_comb_raw$LGR_loc_det <- tags_comb_raw$tagid %in% LGR_loc_det_tags
tags_comb_raw$MCN_loc_det <- tags_comb_raw$tagid %in% MCN_loc_det_tags

# detection not at a recognized colony or primary detection location
tags_comb_raw <- tags_comb_raw %>% 
  filter(reartype!="U") %>% #IMPORTANT
  mutate(non_foc_det=dets>0 & !(AVIAN_recov | prim_loc_det),
         excluded=(trans_statTF|nonLGR_intra_dam_rel),
         ever_seen=dets>0 | AVIAN_recov) 


# LGR_loc_det_tags
####################################################################### #
# ADDING ESTIMATED MIGRATION YEARS FOR FISH DETECTED AT LGR OR MCN
####################################################################### #

table(lubridate::year(obs_comb_raw$mintime))

detyr_LGR <- obs_comb_raw %>% filter(prim_loc_cat=="LGR") %>% 
  group_by(tagid) %>% 
  summarize(estMigyrLGR=lubridate::year(min(mintime)))

detyr_MCN <- obs_comb_raw %>% filter(prim_loc_cat=="MCN") %>% 
  group_by(tagid) %>% 
  summarize(estMigyrMCN=lubridate::year(min(mintime))) #%>%

# nrow(tags_comb_raw)
# head(tags_comb_raw)
tags_comb_raw <- tags_comb_raw %>% left_join(detyr_LGR,by="tagid") %>% left_join(detyr_MCN,by="tagid")

# year of first detection at LGR or MCN following release 
subb_tb1 <- tags_comb_raw %>% 
  filter(!is.na(estMigyrLGR) | !is.na(estMigyrMCN)) # %>%

subb_tb1$min_estMigry <- sapply(1:nrow(subb_tb1),function(ii) {
  min(subb_tb1$estMigyrMCN[ii],
      subb_tb1$estMigyrLGR[ii],na.rm=T)})


tags_comb_raw <- tags_comb_raw %>% left_join(subb_tb1 %>% select(tagid,min_estMigry),by="tagid")
head(tags_comb_raw %>% filter(!is.na(min_estMigry)))

tags_and_obs_comb_raw_ls <- list("tags_comb_raw"=tags_comb_raw,
                                 "obs_comb_raw"=obs_comb_raw)


saveRDS(tags_and_obs_comb_raw_ls,"temp/tags_and_obs_comb_raw_ls9825.rds")


tags_comb <- tags_comb_raw %>% filter(!excluded & reartype!="U")

#### filtering tags and observations ### #
message("Filtering out tags flagged by DART transport filter OR without 'LGRRRR' release site")
tags_comb <- tags_comb_raw[!(tags_comb_raw$trans_statTF | tags_comb_raw$nonLGR_intra_dam_rel),]

# removing tags from observation record
# obs_comb <- obs_comb_raw[obs_comb_raw$tagid %in% tags_comb$"tagid",]
# rm(obs_comb)
obs_comb <- filter(obs_comb_raw,tagid %in% tags_comb$"tagid") %>%
  arrange(tagid,mintime) %>%  
  group_by(tagid) %>% filter(obssite!="GRX") %>% # eliminating duplicate times from GRX
  mutate(detID_raw=seq_along(mintime))

gc()

saveRDS(list("tags_comb"=tags_comb,"obs_comb"=obs_comb),
        "comp_files/tags_and_obs_comb_ls9825.rds")


nrow(obs_comb_raw)
nrow(obs_comb)

