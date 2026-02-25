library(foreign)
library(dplyr)
library(reshape2)

tags_comb <- (readRDS("comp_files/tags_and_obs_comb_ls9825.rds"))$"tags_comb"
obs_comb <- (readRDS("comp_files/tags_and_obs_comb_ls9825.rds"))$"obs_comb"


table(obs_comb$detID_raw)
table(is.na(obs_comb$detID_raw))

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
sub_lgr_rel_obs$event <- ifelse(sub_lgr_rel_obs$eventORIG=="release_only","virt_detection",
                                sub_lgr_rel_obs$eventORIG)
sub_lgr_rel_obs$mintime <- as.POSIXct(ifelse(sub_lgr_rel_obs$event=="virt_detection",
                                             sub_lgr_rel_obs$reltime,sub_lgr_rel_obs$mintime),origin="1970-01-01")

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
# DF_look[DF_look$tagid %in% c("3DD.003D510FB5","3DD.003E29ECB9","3D9.1BF1C0B32E")  ,c("tagid","obssites","diff_days")]

# detection
sub_lgr_det_obs <- obs_comb[obs_comb$relsite!="LGRRRR" & obs_comb$obssite %in% c("GRJ","GRS"),]
sub_mcn_det_obs<- obs_comb[obs_comb$relsite!="LGRRRR" & obs_comb$obssite %in% c("MCJ"),]

# pooled mcnary detections even when LGRRRR is included
# sub_mcn_det_obs<- obs_comb[ obs_comb$obssite %in% c("MCJ"),]

################################################# #
# old location of tag dataset definitions
################################################# #


# obs_rel_grps$mintime <- as.POSIXct(ifelse(obs_rel_grps$event %in% c("virt_detection(0)"),
#                                           obs_rel_grps$reltime,obs_rel_grps$mintime),origin="1970-01-01")


# creating additional rows for treating release as an initial detection event
virt_dets_lgr_relonly <- subset(obs_comb,tagid %in% sub_lgr_rel_obs$tagid & event=="release_only")
virt_dets_lgr_relonly$obssite="LGRRRR"
virt_dets_lgr_relonly$event="virt_detection(0)"
virt_dets_lgr_relonly$detID_raw=0
virt_dets_lgr_relonly$mintime <- as.POSIXct(virt_dets_lgr_relonly$reltime)

  # ifelse(virt_dets_lgr_relonly$event %in% c("virt_detection(0)"),
  #        obs_rel_grps$reltime,obs_rel_grps$mintime),origin="1970-01-01")

all(sub_lgr_rel_obs$detID_raw[sub_lgr_rel_obs$eventORIG=="release_only"])


# LGRRRR releases with subsequent detections
virt_dets_lgr_wsubseq_dets <- subset(obs_comb,tagid %in% sub_lgr_rel_obs$tagid & event!="release_only")
virt_dets_lgr_wsubseq_dets$event="virt_detection(>0)"
nrow(virt_dets_lgr_wsubseq_dets)
nrwcheck <- nrow(virt_dets_lgr_wsubseq_dets) + length(unique(virt_dets_lgr_wsubseq_dets$tagid))

# grabbing only the first row to create a detection surrogate at time of release at LGR
# detID_raw will be wet to zero
virt_dets_lgr_surrog <- subset(virt_dets_lgr_wsubseq_dets, detID_raw==1)
virt_dets_lgr_surrog$detID_raw=0
virt_dets_lgr_surrog$obssiteORIG=virt_dets_lgr_surrog$obssite
virt_dets_lgr_surrog$obssite="LGRRRR"
virt_dets_lgr_surrog$prim_loc_cat="LGRRRR"
virt_dets_lgr_surrog$prim_surface=NA
virt_dets_lgr_surrog$mintime <- as.POSIXct(virt_dets_lgr_surrog$reltime)

virt_dets_lgr_rel_as_det_wsubseq_dets <- bind_rows(virt_dets_lgr_surrog,virt_dets_lgr_wsubseq_dets)

# verifying
(nrow(virt_dets_lgr_rel_as_det_wsubseq_dets)==nrwcheck)


# observations
obs_rel_grps <- bind_rows(
  data.frame(dat_grp="lgr_pooled",virt_dets_lgr_relonly),
  data.frame(dat_grp="lgr_pooled",virt_dets_lgr_rel_as_det_wsubseq_dets),
  data.frame(dat_grp="lgr_pooled",subset(obs_comb,tagid %in% sub_lgr_det_obs$tagid)),
  
  data.frame(dat_grp="lgr_det",subset(obs_comb,tagid %in% sub_lgr_det_obs$tagid)),
  data.frame(dat_grp="mcn_det",subset(obs_comb,tagid %in% sub_mcn_det_obs$tagid)))
nrow(obs_rel_grps)


obs_rel_grps %>% filter(tagid=="222F635313")

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

# obs_rel_grps$mintime <- as.POSIXct(ifelse(obs_rel_grps$event %in% c("virt_detection(0)"),
#                                           obs_rel_grps$reltime,obs_rel_grps$mintime),origin="1970-01-01")

# obs_rel_grps$mintime <- as.POSIXct(ifelse(obs_rel_grps$event %in% c("virt_detection(>0)" ,"virt_detection(0)"),
#                                           obs_rel_grps$reltime,obs_rel_grps$mintime),origin="1970-01-01")


head(obs_rel_grps)


gc()

bt=proc.time()
obs_rel_grps <- obs_rel_grps %>% arrange(dat_grp,esutype,reartype,tagid,mintime)
proc.time()-bt

table(obs_rel_grps$event,obs_rel_grps$dat_grp,obs_rel_grps$esutype)

# OLD AND BAD UNDERCOUNT
# , ,  = SR_Ch1
# lgr_det lgr_pooled mcn_det
# detection          1383675    1383675  993083
# release_only             0          0       0
# virt_detection(>0)       0     214931       0
# virt_detection(0)        0     134566       0
# 
# , ,  = SR_Sock
# lgr_det lgr_pooled mcn_det
# detection           169210     169210   39680
# release_only             0          0       0
# virt_detection(>0)       0          3       0
# virt_detection(0)        0          2       0
# 
# , ,  = SR_Sthd
# lgr_det lgr_pooled mcn_det
# detection           881182     881182  290388
# release_only             1          1       0
# virt_detection(>0)       0     194294       0
# virt_detection(0)        0     159552       0
# table(tagDF_rel_grps$event,tagDF_rel_grps$dat_grp,tagDF_rel_grps$esutype)

obs_rel_grps$date=as.Date(obs_rel_grps$mintime)
obs_rel_grps$year=format(obs_rel_grps$date,"%Y")
obs_rel_grps$id <- 1:nrow(obs_rel_grps) # row index

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

# first detection for LGR and MCN detection data sets
lowest_ids <- tapply(tmp$id,tmp$code,min)

# what row corresponds with the 
# v1_lowest_ids <- obs_rel_grps$id[which(#obs_rel_grps$stage=="J" &
#                                          obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$event=="virt_detection")]
# v2_lowest_ids <- obs_rel_grps$id[which(#obs_rel_grps$stage=="J" & 
#                                          obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$event!="virt_detection" & obs_rel_grps$obssite %in% c("GRS","GRJ"))]

# LGR releases and detections pooled
v1_lowest_ids <- obs_rel_grps$id[obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$obssite %in% c("GRS","GRJ","LGRRRR")]

# v2_lowest_ids <- obs_rel_grps$id[obs_rel_grps$dat_grp=="lgr_pooled" & obs_rel_grps$obssite %in% c("GRS","GRJ","LGRRRR")]
# "virt_detection(0)"

# LGR releases and detections
tmp2 <- obs_rel_grps[obs_rel_grps$id %in% c(v1_lowest_ids),]#,v2_lowest_ids),]
lowest_ids2 <- tapply(tmp2$id,tmp2$code,min)

comb_ids <- c(lowest_ids,lowest_ids2)

# confirming that there are not duplicate rows
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


subbb <- obs_rel_grps %>% filter(tagid %in% (obs_rel_grps %>% filter(detID_raw==0) %>% pull(tagid)))

table(subbb$event,subbb$detID_raw)

# table(subbb$detID_raw==0)
# View(obs_rel_grps %>% filter(tagid %in% (obs_rel_grps %>% filter(detID_raw==0) %>% pull(tagid))))
# forcing observations without any detections initial detections to be lifestage J

# these are the release events/intial detections for fish  release fish that were never seen again
obs_rel_grps$stage[obs_rel_grps$detID_raw==0]="J"

table(obs_rel_grps$stage)

# obs_rel_grps$rel
subbb2 <- obs_rel_grps %>% group_by(esutype,reartype,relyr,tagid) %>% 
  summarize(stage_v=paste(stage,collapse=","))
tb_JA_hist_all <- sort(table(subbb2$stage_v),decreasing = T)
# more than 99% of the time its all Js
tb_JA_hist_all[cumsum(tb_JA_hist_all)/sum(tb_JA_hist_all)<=0.999]
strsplit(names(tb_JA_hist_all),split = ",")


earliest_stage_mat <- t(sapply(names(tb_JA_hist_all),function(x){
  vv=strsplit(x,split=",")[[1]]
  jj=ifelse(is.finite(min(which(vv=="J"))),min(which(vv=="J")),NA)
  aa=ifelse(is.finite(min(which(vv=="A"))),min(which(vv=="A")),NA)
  j_after_a=any(which(vv=="J") > min(which(vv=="A")))
  # jj=ifelse(is.finite(min(which(vv=="J"))),min(which(vv=="J")),NA)
  c(jj,aa,j_after_a)
  }))

colnames(earliest_stage_mat) <- c("J","A","j_after_a")
earliest_stageDF <- data.frame(earliest_stage_mat,seq_stg=rownames(earliest_stage_mat))
rownames(earliest_stageDF) <- NULL
earliest_stageDF$Jb4A <- earliest_stageDF$J<earliest_stageDF$A

earliest_stageDF$jb4_ <- ifelse(!is.na(earliest_stageDF$A),earliest_stageDF$J<earliest_stageDF$A,NA)
# ifelse(!is.na(earliest_stageDF$J),earliest_stageDF$J<earliest_stageDF$A,NA)


subbb3 <- obs_rel_grps %>% group_by(esutype,reartype,relyr,tagid) %>%
  filter(!is.na(prim_loc_cat)) %>%
  summarize(
    stage_v=paste(stage,collapse=","))
tb_JA_hist_prim_loc <- sort(table(subbb3$stage_v),decreasing = T)
tb_JA_hist_prim_loc[cumsum(tb_JA_hist_prim_loc)/sum(tb_JA_hist_prim_loc)<=0.999]

names(tb_JA_hist_prim_loc)


############################################################################# #
# # investigating the order of 'J's and 'A's grinds everything to a halt
# subbb3 <- obs_rel_grps %>% group_by(esutype,reartype,relyr,tagid) %>% 
#   filter(!is.na(prim_loc_cat)) %>%
#   summarize(
#     # paste(which(stage=="J"),collapse=","),
#     # paste(which(stage=="A"),collapse=","),
#     min_indJ=min(which(stage=="J")),
#     min_indA=min(which(stage=="A")),
#     stage_v=paste(stage,collapse=","))
# 
############################################################################# #

head(subbb3)
table(subbb3$stage_v)

# table(obs_rel_grps$stage)
table(is.na(obs_rel_grps$stage))

# table(subbb2 %>% pull(stage_v))
############################################################### #
# table of definitive detctions times (based on comb_ids)
############################################################### #


#################################################### #
# tagid
tagDF_rel_grps <- rbind(
  data.frame(dat_grp="lgr_pooled",tags_comb[tags_comb$tagid %in% sub_lgr_rel_obs$tagid,]),
  data.frame(dat_grp="lgr_pooled",tags_comb[tags_comb$tagid %in% sub_lgr_det_obs$tagid,]),
  data.frame(dat_grp="lgr_det",tags_comb[tags_comb$tagid %in% sub_lgr_det_obs$tagid,]),
  # data.frame(dat_grp="mcn_pooled",tags_comb[tags_comb$tagid %in% sub_mcn_det_obs$tagid,]) # would need more modification
  data.frame(dat_grp="mcn_det",tags_comb[tags_comb$tagid %in% sub_mcn_det_obs$tagid,]))
tagDF_rel_grps$relyr <- as.numeric(format(tagDF_rel_grps$reltime,"%Y"))

table(tagDF_rel_grps$event)
table(tagDF_rel_grps$event)


######################## #

# breakdown of tags present in each dat_grp
table(table(tagDF_rel_grps$tagid))
#       1       2       3 
# 1075116  975316  149009 
# 1110146 1083828  151194 revised is the same

#################################################### #


# table of definitive detection times for tags
defin_detDF <- obs_rel_grps %>% 
  filter(id %in% comb_ids) %>% 
  filter(stage=="J") %>% # only juvenile initial detection allowed
  mutate(defin_det_time=mintime ) %>%
  rename(defin_det_yr=year) %>% 
  select(dat_grp,esutype,code,tagid,stage,mcn_det,lgr_det,event,obssite,defin_det_time,defin_det_yr,mintime) %>%
  left_join(tagDF_rel_grps %>% select(tagid,dat_grp,esutype)) #%>% #,hist_type,reartype,length,markdate,relsite,relyr,reltime,dets,trans_statTF))

defin_detDF$defin_half_day_det <- lubridate::floor_date(defin_detDF$defin_det_time,unit = "12 hours")

# table(defin_detDF$dat_grp,defin_detDF$obssite)
# data.frame(table(defin_detDF$dat_grp,defin_detDF$obssite))
# 
# table(defin_detDF$obssite)
# 
# nrow(defin_detDF)
# table(table(defin_detDF$code))

########################################################### #



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

################################################################################ #
# Reviewing observations without definitive detections
################################################################################ #

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

# viewing the 
subb_all_good %>% filter(stage!="J")
subb_all_good %>% filter(stage=="J")

################################################################################ #

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

######################## #
# Defining 'init_det'
######################## #
obs_rel_grps2$init_det <- obs_rel_grps2$mintime==obs_rel_grps2$defin_det_time
table(is.na(obs_rel_grps2$init_det))

# table(obs_rel_grps2[is.na(obs_rel_grps2$init_det),]$tagid,)
obs_rel_grps2$init_det[is.na(obs_rel_grps2$init_det)]=FALSE
table(obs_rel_grps2$init_det)
# head(LGR_DH_matchDF)


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
nrow(obs_rel_grps2)

# now 3731384, was 3472775 # total codes  
length(unique(obs_rel_grps2$code))
# now 3730094, was 3471774 # total codes with a definitive detection assignmet
length(unique(obs_rel_grps2$code[obs_rel_grps2$det_init]))

saveRDS(obs_rel_grps,"comp_files/obs_rel_grps.rds")
saveRDS(defin_detDF,"comp_files/defin_detDF.rds")
# before removing negative values relative to the definitive detection
saveRDS(obs_rel_grps2,"comp_files/obs_rel_grps_2_9825.rds")


