load("temp/data_comp_WS_9825.Rdata")

head(raw_DF)
table(tagDF$relsite)

bt_all <- proc.time()

library(foreign)
library(dplyr)
library(reshape2)

bt_total <- proc.time()
# source("functions.R")
source("02c_load_spat_dat.R")

#### Loading and combining raw obs and tag files ####


in_dir="comp_files/data_9825_comp"
if(!dir.exists(in_dir)){dir.create(in_dir,recursive = T)}

# separates obs and tags within workspace
ESU_nms=c("SR_Ch1","SR_Sthd","SR_Sock")

obs_comb_raw=raw_DF
tags_comb_raw <- data.frame(obs_comb_raw[match(tagDF$tagid,obs_comb_raw$tagid),c("esutype","event","species","run")],tagDF)
names(tags_comb_raw)[2] <- "hist_type"

rm(raw_DF)
rm(tagDF)
table(obs_comb_raw$obssite)

# doubted this would work as is
# raw_TWX_DF <- readRDS("temp/data_9823_comp_SI_test_TWX/raw_TWX_DF.RDS")


raw_TWX_DF <- readRDS("temp/data_9825_TWX/raw_TWX_DF_9825.rds")



all(raw_TWX_DF$tagid %in% c(tags_comb_raw$tagid))
head(tags_comb_raw)
all(raw_TWX_DF$tagid %in% c(obs_comb_raw$tagid))
head(raw_TWX_DF)
obs_comb_rawNOTWX <- obs_comb_raw


############################ #
# ADDING TWX DETAILS  
############################ #

bt=proc.time()
obs_comb_raw <- obs_comb_raw %>% left_join(raw_TWX_DF %>% select(tagid,obssite,obsdetail,mintime))
proc.time()-bt

TWX_tags <- unique(obs_comb_raw %>% filter(!is.na(obs_comb_raw$obsdetail)) %>% pull(tagid))
obs_comb_TWX <- obs_comb_raw %>% filter(tagid %in% TWX_tags)
table(obs_comb_raw$obsdetail,obs_comb_raw$migryr)
table(obs_comb_TWX$obsdetail,obs_comb_TWX$migryr)
data.frame(table(obs_comb_raw$obssite)[order(names(table(obs_comb_raw$obssite)))])

######################################################################################## #
#### creating tagid lists Adding TRUE/FALSE columns to tag data for filtering ####
######################################################################################## #

data.frame(table(obs_comb_raw$obssite))

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

#### filtering tags and observations ####

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



#### Creating LGR_pooled release group ####

# subsetting fish officially released at LGR and without transport code
tagDF_sub_lgr_rel <- tags_comb[tags_comb$lgr_rel,]
tags_sub_lgr_rel_tags <- tagDF_sub_lgr_rel$tagid

# redefining "release-only" fish as virtual detections
tagDF_sub_lgr_rel$event <- ifelse(tagDF_sub_lgr_rel$event=="release_only","detection",tagDF_sub_lgr_rel$event)

# substitute release site name of interrogation site name MMR site name
# tagDF_sub_lgr_rel$obssite[tagDF_sub_lgr_rel$event=="virt_detection"]="LGRRRR"


sub_lgr_rel_obs <- obs_comb[obs_comb$relsite=="LGRRRR",]
sub_lgr_rel_obs$obssiteORIG <- sub_lgr_rel_obs$obssite
sub_lgr_rel_obs$obssite <- ifelse(sub_lgr_rel_obs$obssiteORIG=="","LGRRRR",sub_lgr_rel_obs$obssiteORIG)

sub_lgr_rel_obs$eventORIG <- sub_lgr_rel_obs$event
sub_lgr_rel_obs$event <- ifelse(sub_lgr_rel_obs$eventORIG=="release_only","virt_detection",sub_lgr_rel_obs$eventORIG)
sub_lgr_rel_obs$mintime <- as.POSIXct(ifelse(sub_lgr_rel_obs$event=="virt_detection",sub_lgr_rel_obs$reltime,sub_lgr_rel_obs$mintime),origin="1970-01-01")

# a row must be added for LGRRRR releases that actually had downstream detections
# all(sub_lgr_rel_obs$relsite=="LGRRRR")
ggg <- sub_lgr_rel_obs %>% group_by(esutype,eventORIG,stage,tagid) %>%
  summarize(
    LGRRRR_any=any(obssite=="LGRRRR"),
    n_dets=length(mintime))

table(sub_lgr_rel_obs$detID_raw)
# table(ggg[ggg$LGRRRR_any,]$eventORIG)
# table(is.na(ggg[ggg$LGRRRR_any,]$eventORIG))


######################################################################################################## #

# virtual releases at LGRRRR rarely had any detections at GRJ or GRS following release

# there are 67 tags that have a LGRRRR and at EITHER GRJ or GRS
sub <- sub_lgr_rel_obs[sub_lgr_rel_obs$relsite== "LGRRRR" & sub_lgr_rel_obs$obssite %in% c("GRJ","GRS"),]
length(unique(sub$tagid))

sub2 <- sub %>% 
  mutate(time_diff=difftime(mintime,reltime,units="days")) %>%
  select(tagid,reltime,mintime,obssite,time_diff) %>%
  arrange(time_diff)
gr_1day_diff <- sub2 %>%# filter(time_diff>1) %>% 
  pull(tagid)

DF_look <- sub_lgr_rel_obs[sub_lgr_rel_obs$tagid %in% gr_1day_diff,] %>% 
  group_by(esutype,reartype,tagid,relsite) %>% #,dat_grp
  summarize( obssites=paste(
    ifelse(obssite_prim[order(mintime)],obssite[order(mintime)],
           tolower(obssite[order(mintime)]))
    ,collapse=" -> "),
    
    stages=paste(stage[order(mintime)],collapse=" -> "),
    det_years=paste(format(mintime,"%y")[order(mintime)],collapse=" -> "),
    diff_days=paste(
      round(as.numeric(difftime(mintime[order(mintime)][2:(length(mintime))],
                                mintime[order(mintime)][1:(length(mintime)-1)],units = "days")))
      ,collapse="  "))

DF_look[,c("tagid","obssites","diff_days")]
DF_look[DF_look$tagid %in% c("3DD.003D510FB5","3DD.003E29ECB9","3D9.1BF1C0B32E")  ,c("tagid","obssites","diff_days")]





# detection
sub_lgr_det_obs <- obs_comb[obs_comb$relsite!="LGRRRR" & obs_comb$obssite %in% c("GRJ","GRS"),]
sub_mcn_det_obs<- obs_comb[obs_comb$relsite!="LGRRRR" & obs_comb$obssite %in% c("MCJ"),]

# tagid
tagDF_rel_grps <- rbind(
  data.frame(dat_grp="lgr_pooled",tags_comb[tags_comb$tagid %in% sub_lgr_rel_obs$tagid,]),
  data.frame(dat_grp="lgr_pooled",tags_comb[tags_comb$tagid %in% sub_lgr_det_obs$tagid,]),
  data.frame(dat_grp="lgr_det",tags_comb[tags_comb$tagid %in% sub_lgr_det_obs$tagid,]),
  data.frame(dat_grp="mcn_det",tags_comb[tags_comb$tagid %in% sub_mcn_det_obs$tagid,]))
tagDF_rel_grps$relyr <- as.numeric(format(tagDF_rel_grps$reltime,"%Y"))

table(tagDF_rel_grps$event)
table(tagDF_rel_grps$event)


# creating additional rows for treating release as an initial detection event
virt_dets_lgr_relonly <- subset(obs_comb,tagid %in% sub_lgr_rel_obs$tagid & event=="release_only")
# grabing just the first row of the detection record 
virt_dets_lgr <- subset(obs_comb,tagid %in% sub_lgr_rel_obs$tagid & event!="release_only" & detID_raw==1)

# adding special event label
virt_dets_lgr$event="virt_detection(>0)"
virt_dets_lgr$obssiteORIG=virt_dets_lgr$obssite

virt_dets_lgr_relonly$obssite="LGRRRR"
virt_dets_lgr$obssite="LGRRRR"
virt_dets_lgr_relonly$event="virt_detection(0)"
virt_dets_lgr_relonly$detID_raw=0
virt_dets_lgr$detID_raw=0
all(sub_lgr_rel_obs$detID_raw[sub_lgr_rel_obs$eventORIG=="release_only"])


# observations
obs_rel_grps <- bind_rows(
  data.frame(dat_grp="lgr_pooled",virt_dets_lgr_relonly),
  data.frame(dat_grp="lgr_pooled",virt_dets_lgr),
  data.frame(dat_grp="lgr_pooled",subset(obs_comb,tagid %in% sub_lgr_det_obs$tagid)),
  data.frame(dat_grp="lgr_det",subset(obs_comb,tagid %in% sub_lgr_det_obs$tagid)),
  data.frame(dat_grp="mcn_det",subset(obs_comb,tagid %in% sub_mcn_det_obs$tagid)))
nrow(obs_rel_grps)


# sub <- obs_rel_grps[obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$relsite=="LGRRRR",]
# sub_lgr_rel_obs <- obs_comb[obs_comb$relsite=="LGRRRR",]
# sub_lgr_rel_obs$obssiteORIG <- sub_lgr_rel_obs$obssite
# sub_lgr_rel_obs$obssite <- ifelse(sub_lgr_rel_obs$obssiteORIG=="","LGRRRR",sub_lgr_rel_obs$obssiteORIG)
# sub_lgr_rel_obs$eventORIG <- sub_lgr_rel_obs$event
# sub_lgr_rel_obs$event <- ifelse(sub_lgr_rel_obs$eventORIG=="release_only","virt_detection",sub_lgr_rel_obs$eventORIG)
# sub_lgr_rel_obs$mintime <- as.POSIXct(ifelse(sub_lgr_rel_obs$event=="virt_detection",sub_lgr_rel_obs$reltime,sub_lgr_rel_obs$mintime),origin="1970-01-01")

# obs_rel_grps$obssiteORIG <- obs_rel_grps$obssite
# obs_rel_grps$obssite <- ifelse(obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$obssiteORIG=="","LGRRRR",obs_rel_grps$obssiteORIG)

# replacing release time and adding a Juvenile label
# obs_rel_grps$stage[obs_rel_grps$obssite=="LGRRRR"]="J"
obs_rel_grps$mintime <- as.POSIXct(ifelse(obs_rel_grps$event %in% c("virt_detection(>0)" ,"virt_detection(0)"),obs_rel_grps$reltime,obs_rel_grps$mintime),origin="1970-01-01")
head(obs_rel_grps)


gc()

bt=proc.time()
obs_rel_grps <- obs_rel_grps %>% arrange(dat_grp,esutype,reartype,tagid,mintime)
proc.time()-bt


table(obs_rel_grps$event,obs_rel_grps$dat_grp,obs_rel_grps$esutype)
table(tagDF_rel_grps$event,tagDF_rel_grps$dat_grp,tagDF_rel_grps$esutype)

obs_rel_grps$date=as.Date(obs_rel_grps$mintime)
obs_rel_grps$year=format(obs_rel_grps$date,"%Y")
obs_rel_grps$id <- 1:nrow(obs_rel_grps)

# finding rows with a MCN and LGR detection
obs_rel_grps$mcn_det <- obs_rel_grps$dat_grp=="mcn_det" & obs_rel_grps$obssite %in% c("MCJ")
obs_rel_grps$lgr_det <- obs_rel_grps$dat_grp== c("lgr_det") & obs_rel_grps$obssite %in% c("GRS","GRJ")

obs_rel_grps$code=paste(obs_rel_grps$dat_grp,obs_rel_grps$esutype,obs_rel_grps$reartype,obs_rel_grps$tagid)

# subsetting to only the earliest
# tmp <- obs_rel_grps[#obs_rel_grps$stage=="J" & 
#                       obs_rel_grps$dat_grp!="lgr_pooled" & (obs_rel_grps$mcn_det | obs_rel_grps$lgr_det),]

tmpA <- obs_rel_grps[#obs_rel_grps$stage=="J" & 
  obs_rel_grps$dat_grp=="lgr_det" & (obs_rel_grps$obssite %in% c("GRS","GRJ")),]
tmpB <- obs_rel_grps[#obs_rel_grps$stage=="J" & 
  obs_rel_grps$dat_grp=="mcn_det" &  (obs_rel_grps$obssite %in% c("MCJ")),]
tmp <- rbind(tmpA,tmpB)

lowest_ids <- tapply(tmp$id,tmp$code,min)

# what row corresponds with the 
# v1_lowest_ids <- obs_rel_grps$id[which(#obs_rel_grps$stage=="J" &
#                                          obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$event=="virt_detection")]
# v2_lowest_ids <- obs_rel_grps$id[which(#obs_rel_grps$stage=="J" & 
#                                          obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$event!="virt_detection" & obs_rel_grps$obssite %in% c("GRS","GRJ"))]

v1_lowest_ids <- obs_rel_grps$id[obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$obssite %in% c("GRS","GRJ","LGRRRR")]

# v2_lowest_ids <- obs_rel_grps$id[obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$obssite %in% c("GRS","GRJ","LGRRRR")]
# 
# "virt_detection(0)"


tmp2 <- obs_rel_grps[obs_rel_grps$id %in% c(v1_lowest_ids),]#,v2_lowest_ids),]
lowest_ids2 <- tapply(tmp2$id,tmp2$code,min)

comb_ids <- c(lowest_ids,lowest_ids2)

table(duplicated(comb_ids))

# FALSE 
# 3472775 vs. 3198946 When stage is eliminated from the subset
# 3472775-3198946 =~270,000
# 
# bb <- obs_rel_grps %>%  filter(id %in% comb_ids)
# table(bb$dat_grp,bb$event)
# 
# obs_comb %>% group_by(tagid,obssite) %>%
#   summarize(ndups=length(duplicated(mintime))) %>%
#   filter(ndups>0)
# 
# table(obs_rel_grps[obs_rel_grps$obssite=="LGRRRR",]$stage)


obs_rel_grps$stage[obs_rel_grps$detID_raw==0]="J"

# table of definitive detection times for tags
defin_detDF <- obs_rel_grps %>% 
  filter(id %in% comb_ids) %>% 
  filter(stage=="J") %>% # only juvenile
  mutate(defin_det_time=mintime ) %>%
  rename(defin_det_yr=year) %>% 
  select(dat_grp,esutype,code,tagid,stage,mcn_det,lgr_det,event,obssite,defin_det_time,defin_det_yr,mintime) %>%
  left_join(tagDF_rel_grps %>% select(tagid,dat_grp,esutype)) #%>% #,hist_type,reartype,length,markdate,relsite,relyr,reltime,dets,trans_statTF))

defin_detDF$defin_half_day_det <- lubridate::floor_date(defin_detDF$defin_det_time,unit = "12 hours")

table(defin_detDF$dat_grp,defin_detDF$obssite)
data.frame(table(defin_detDF$dat_grp,defin_detDF$obssite))

table(defin_detDF$obssite)

nrow(defin_detDF)
table(table(defin_detDF$code))


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
# 1.9 million starting detections

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



