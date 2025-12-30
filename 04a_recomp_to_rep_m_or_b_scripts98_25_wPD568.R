library(ggplot2)
library(reshape2)
library(dplyr)
# source("functions.R")

obs_rel_grps2 <- readRDS("comp_files/obs_rel_grps_2_9825.rds")
tagDF_rel_grps <- readRDS("comp_files/tagDF_rel_grps_9825.rds")


# table(obs_rel_grps2$dat_grp,obs_rel_grps2$prim_loc_cat)
# table(obs_rel_grps2$dat_grp,obs_rel_grps2$obssite_prim)
# table(obs_rel_grps2$defin_det_yr,obs_rel_grps2$obsdetail)



# tag summary table
tagsum2 <- obs_rel_grps2 %>%
  group_by(dat_grp,esutype,reartype,tagid,code) %>%
  mutate(detID_raw=seq_along(mintime)) %>%
  summarize(
    defin_det=unique(detID_raw[init_det]),
    n_defins=sum(det_init),
    n_dets=length(det_init)) 

# eliminates all tags that do not have a detection at a definitive location (e.g., LGR and MCN)
bt=proc.time()
obs_rel_grps3<- obs_rel_grps2 %>% filter(!unassagn) %>% 
  left_join(tagsum2)
proc.time()-bt

# creating version of detID at the code level 
#VERY IMPORTANT STEP HERE THAT PREVENTS LGRRRRs from being dropped
obs_rel_grps3$detID_defin[obs_rel_grps3$obssite!="LGRRRR"] <- 
  obs_rel_grps3$detID_raw[obs_rel_grps3$obssite!="LGRRRR"]-obs_rel_grps3$defin_det[obs_rel_grps3$obssite!="LGRRRR"]

# by code summary (codes are combinations of groups and tags)
tagsum3 <- obs_rel_grps3 %>% group_by(code) %>%
  summarize(dets_b4=length(detID_defin<0),
            det_after=length(detID_defin<0))

# ~2.2 million unique tags
length(unique(obs_rel_grps3$tagid))

# ~3.5 million tag histories
table(tagsum2$n_defins==1)

#VERY IMPORTANT STEP HERE
obs_rel_grps3$defin_det_yr[obs_rel_grps3$obssite=="LGRRRR"]
obs_rel_grps3$detID_defin[obs_rel_grps3$obssite=="LGRRRR"]=0 # an index with zero describing the definitive detection event


# Only including the definitive detection and detections that occured afterwards
obs_rel_grps4 <- obs_rel_grps3 %>% 
  filter(defin_det_yr %in% 1998:2025 & detID_defin>=0) %>%
  group_by(code)

foc_obssites <- c("LGRRRR","B2J","BCC","ESX","GRJ","GRS","MCJ","PD5","PD6","PD7","PD8","TWX")

# were combining will happen
obs_rel_grps5 <- obs_rel_grps4 %>% filter(obssite %in% foc_obssites) 
obs_rel_grps5$prim_loc_cat[obs_rel_grps5$obssite=="LGRRRR"]="LGR"
obs_rel_grps5 <- obs_rel_grps5[!(obs_rel_grps5$prim_loc_cat=="LGR" & obs_rel_grps5$dat_grp=="mcn_det"),]

obs_rel_grps5$det_int_days
obs_rel_grps6 <- obs_rel_grps5 %>%  filter(as.numeric(det_int_days)<(365/2)) %>% arrange(dat_grp,esutype,defin_det_yr,code,mintime)
obs_rel_grps6$code2=paste(obs_rel_grps6$code,obs_rel_grps6$prim_loc_cat)

##################################################### #
# obssite level summary before removal of duplicates
##################################################### #

obssite_summ_tab <- obs_rel_grps6 %>% group_by(dat_grp,esutype,defin_det_yr,code) %>% 
  summarize(DH_label=paste(obssite,collapse=" -> ")) %>%
  group_by(dat_grp,esutype,defin_det_yr,DH_label) %>%
  summarize(
    n_dets=length(code),
    n_codes=length(unique(code)))

################################################# #
# simpified to just the general location name
################################################# #

# table categorizing times
time_cat <- obs_rel_grps6 %>% 
  filter(init_det) %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>%
  summarize(frst=min(defin_det_time),
            lst=max(defin_det_time),
            perc1=quantile(defin_det_time,c(0.005)),
            perc99=quantile(defin_det_time,c(0.995)),
            perc5=quantile(defin_det_time,c(0.0275)),
            perc95=quantile(defin_det_time,c(0.975))) %>%
  mutate(frst_bin=lubridate::floor_date(frst,unit = "12 hours"),
         last_bin=lubridate::floor_date(lst,unit = "12 hours"),
         half_days=as.numeric(difftime(last_bin,frst_bin,"days"))*2,
         days=half_days/2,
         grp_code=paste(dat_grp,esutype,reartype,defin_det_yr))

table(time_cat$defin_det_yr)


obs_rel_grps6 <- obs_rel_grps6 %>% mutate(grp_code=paste(dat_grp,esutype,reartype,defin_det_yr))
obs_rel_grps6$before_strt <- obs_rel_grps6$defin_det_time_grp<time_cat$perc1[match(obs_rel_grps6$grp_code,time_cat$grp_code)]
obs_rel_grps6$after_end <- obs_rel_grps6$defin_det_time_grp>time_cat$perc99[match(obs_rel_grps6$grp_code,time_cat$grp_code)]
obs_rel_grps6$within <- !(obs_rel_grps6$before_strt | obs_rel_grps6$after_end)

# breakdown of what is excluded
withn_brkdwn_tab <- obs_rel_grps6 %>% filter(init_det) %>% group_by(dat_grp,esutype,defin_det_yr) %>%
  summarize(
    # obs_rel_grps6
    kept=length(which(within)),
    total=length(within)) %>% 
  mutate(excluded=total-kept,
         retained=kept/total)

summary(withn_brkdwn_tab$excluded)
# between 6 and 500 tags excluded
summary(as.numeric(difftime(obs_rel_grps6$mintime[obs_rel_grps6$before_strt],obs_rel_grps6$defin_det_time_grp[obs_rel_grps6$before_strt],units = "days")))
summary(as.numeric(difftime(obs_rel_grps6$mintime[obs_rel_grps6$after_end],obs_rel_grps6$defin_det_time_grp[obs_rel_grps6$after_end],units = "days")))

# last 1% of tag detections occur up to 2.5 mnths before and  up to 5.5 months after

difftime(obs_rel_grps6$mintime,obs_rel_grps6$defin_det_time_grp,"days")


# filtering down to only those detections that occured within the middle 99%
obs_rel_grps7 <- obs_rel_grps6 %>%
  filter(within,init_det) %>%
  mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"))

time_cat$off_strt=lubridate::floor_date(time_cat$perc1,unit = "12 hours")
time_cat$off_end=lubridate::floor_date(time_cat$perc99,unit = "12 hours")

# late March through mid July
off_used <- time_cat[,c("dat_grp","esutype","defin_det_yr","reartype","grp_code","off_strt","off_end")]
off_used$tbtw <- difftime(off_used$off_end,off_used$off_strt,"days")


# 267 groups
nrow(off_used)

table(obs_rel_grps7$grp_code %in% off_used$grp_code)
table( off_used$grp_code %in% obs_rel_grps7$grp_code)

# mcnary wild sockeye detections
# off_used[!off_used$grp_code %in% obs_rel_grps7$grp_code,]
# mcn_det SR_Sock W 2022

off_used <- off_used[off_used$grp_code %in% obs_rel_grps7$grp_code,]

source("R/make_bin_tab.R")


ii=1
test <- make_bin_tab(strt=off_used$off_strt[ii],
                     end=off_used$off_end[ii],
                     interv="month",
                     row_data=off_used[ii,c(1:5)])
test


off_used[112,]

bt=proc.time()
sub_ls=out_ls=list()
bin_tab_ls_comb=list()
running_id_val <- 0
for( ii in 1:nrow(off_used)){
# for( ii in 425:nrow(off_used)){
  if(ii %in% seq(1,nrow(off_used),20)){message(paste(ii,"of",nrow(off_used)))}
  bin_list_ls=list()
  
  # the final been includes the 99th percentile
  bin_tab_ls=lapply(c("day","days3.5","week1","weeks2","month"),
                    function(x){make_bin_tab(strt=off_used$off_strt[ii],
                                             end=off_used$off_end[ii],
                                             interv=x,
                                             row_data =off_used[ii,c(1:5)] )})
  
  
  bin_list<- lapply(bin_tab_ls,function(x){
    # all the starts and the last end
    c(x$bin_strt,x$bin_end[length(x$bin_end)])})
  names(bin_list)=c("day","days3.5","week1","weeks2","month")
  
  ref_DF <- do.call(rbind,lapply(bin_tab_ls,function(x){
    data.frame(grp_code=x$grp_code,bin=x$bin,binID=x$binID)}))
  ref_DF$unq_binID=running_id_val + 1:nrow(ref_DF)
  running_id_val <- max(ref_DF$unq_binID)
  ref_DF$bin_code=paste(ref_DF$grp_code,ref_DF$bin,ref_DF$binID)
  bin_tab_ls_comb[[ii]] <- data.frame(ref_DF,do.call(rbind,bin_tab_ls))
  
  sub_ls[[ii]] <- obs_rel_grps7[obs_rel_grps7$grp_code==off_used$grp_code[ii],]
  
  # definitive detection times exposed to bin list
  cat_mat <- sapply(bin_list,function(x){cut(sub_ls[[ii]]$defin_det_time,breaks=x,labels = F)})
  # mx_test=cbind(cat_mat,apply(cat_mat,2,function(x){x==max(x)}))
  
  # speccial case when cat_mat had only 1 row
  if(ii %in% c(112,280,424)){
    
    tmp_df <- data.frame(matrix(cat_mat,nrow=1))
    colnames(tmp_df)=names(cat_mat)
    
    out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
                               tmp_df)
  } else{
    out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
                               cat_mat)
  }
  
  # out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
  #                            cat_mat)
  
}
proc.time()-bt # takes ~ 30 seconds


bt=proc.time()
bin_tab_ls_combDF <- do.call(rbind,bin_tab_ls_comb)
obs_rel_grps8 <- do.call(rbind,out_ls) #%>% left_join(bin_tab_ls_combDF %>% select(grp_code,bin_end,partial,ind_bin_hours))
proc.time()-bt # takes ~ 6.5 min

# obs_rel_grps9$grp_code
# bin_tab_ls_combDF


nrow(obs_rel_grps7) #no difference
nrow(obs_rel_grps8)


# getting rid of duplicates in the definitive detection row
# obs_rel_grps9 <- obs_rel_grps6[!duplicated(obs_rel_grps6$code2),]

################################################### #

# for a CJS analysis there shouldn't be duplicate detections
# at the same site. This code eliminates all the second detection events
# at the primary locations

obs_rel_grps9 <- obs_rel_grps6 %>%
  filter(within) %>%
  mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"))

nrow(obs_rel_grps9)
obs_rel_grps9 <- obs_rel_grps9[!duplicated(obs_rel_grps9$code2),]

nrow(obs_rel_grps9)



# 3920377-3920251 #126 duplicates

# obs_rel_grps9 <- obs_rel_grps6[!duplicated(obs_rel_grps6$code2),]


# names(obs_rel_grps9)
# obs_rel_grps9$defin_det_time
# obs_rel_grps9$mintime
# 
# obs_rel_grps9 %>% select(code,defin_det_yr,defin_det_time,defin_det_time_grp,mintime)
# obs_rel_grps8 %>% select(code,defin_det_yr,defin_det_time,defin_det_time_grp,mintime)
# 
# are there any codes in 9 that are not in 8
# sapply(obs_rel_grps9$code,function(x) x %in% obs_rel_grps8$code)

# trying to get bin assignments from 8 onto 9




dh_tab <- obs_rel_grps9 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code) %>%
  summarize(DH_label=paste(prim_loc_cat,collapse=" -> "))


gc()

head(dh_tab)

# look for transition patterns that should be omitted because they are probably adults 
# manually replace as neccessary
table(dh_tab$DH_label) 

# pre-loaded tables describing detection histories for 2 locations
LGR_DH_matchDF
MCN_DH_matchDF



bt=proc.time()

lgr_dh_tab <- dh_tab %>% filter(dat_grp %in% c("lgr_det","lgr_pooled")) %>% left_join(LGR_DH_matchDF)
mcn_dh_tab <- dh_tab %>% filter(dat_grp=="mcn_det") %>% left_join(MCN_DH_matchDF)


################################################### #
# nas removed because dh_tab and obs_rel_grps9 have all the rows and 
# obs_rel_grps8 is filtered to only within init_det
# but obs_rel_grps8 has temporal bin assignments


# identifying codes(grp_code + tagid) in obs_rel_grps9 that 
mtc1 <- match(lgr_dh_tab$code,obs_rel_grps8$code)
mtc2 <- match(mcn_dh_tab$code,obs_rel_grps8$code)
table(is.na(mtc1))
table(is.na(mtc2))


head(obs_rel_grps8)


lgr_dh_tab2 <- data.frame(lgr_dh_tab[!is.na(mtc1),],obs_rel_grps8[mtc1[!is.na(mtc1)],c("relsite","day","days3.5","week1","weeks2","month")])
mcn_dh_tab2 <- data.frame(mcn_dh_tab[!is.na(mtc2),],obs_rel_grps8[mtc2[!is.na(mtc2)],c("relsite","day","days3.5","week1","weeks2","month")])

lgr_dh_tab2$virt_det <- lgr_dh_tab2$relsite=="LGRRRR"
mcn_dh_tab2$virt_det <- mcn_dh_tab2$relsite=="LGRRRR"

proc.time()-bt # takes ~ 6.5 mins

lgr_dh_tab2
mcn_dh_tab2
gc()

nrow(bin_tab_ls_combDF)
nrow(bin_tab_ls_combDF %>% filter(bin=="day" & binID %in% 1:(length(unique(bin_tab_ls_combDF$defin_det_yr)))))
# 15*

yr_binsDF <- bin_tab_ls_combDF %>% 
  group_by(dat_grp,esutype,defin_det_yr,reartype,grp_code,official_strt,official_end) %>%
  summarize(bins_in_grp=length(binID),
            subyear_bin_types=length(unique(bin))) %>%
  mutate(binID=as.numeric(defin_det_yr)-min(as.numeric(bin_tab_ls_combDF$defin_det_yr))+1,
         bin="year",
         bin_strt=official_strt,
         bin_end=official_end,
         partial=FALSE,
         ind_bin_hours=as.numeric(difftime(official_end,official_strt,units = "hours")),
         full_bin_hours=ind_bin_hours,
         ind_bin_days=ind_bin_hours/24
  )

bin_tab_ls_combDFwYR <- bind_rows(bin_tab_ls_combDF,yr_binsDF) %>% 
  select(-bins_in_grp,-subyear_bin_types) %>% 
  select(-binID.1,-bin.1,-grp_code.1)

# last 267 rows are years
bin_tab_ls_combDFwYR$unq_binID <- 1:nrow(bin_tab_ls_combDFwYR)


mcn_dh_tab2


saveRDS(obs_rel_grps9,"comp_files/obs_rel_grps9_9823_wPD568.rds")
saveRDS(obs_rel_grps8,"comp_files/obs_rel_grps8_9823_wPD568.rds")



saveRDS(lgr_dh_tab2,"comp_files/lgr_dh_tab2_9823_wPD568.rds")
saveRDS(mcn_dh_tab2,"comp_files/mcn_dh_tab2_9823_wPD568.rds")
# saveRDS(DF,"temp/diff_time_frst_to_99p_9823_wPD568.rds")
# saveRDS(bin_tab_ls_combDF,"temp/bin_tab_ls_combDF.rds")

saveRDS(bin_tab_ls_combDFwYR,"comp_files/bin_tab_ls_combDFwYR_wPD568.rds")


