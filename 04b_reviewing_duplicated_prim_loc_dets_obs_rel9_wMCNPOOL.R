library(ggplot2)
library(reshape2)
library(dplyr)
# source("functions.R")


obs_rel_grps2 <- readRDS("comp_files/obs_rel_grps_2_9825_wMCNPOOL.rds")
#  <- readRDS("temp/obs_rel_grps_2_9825.rds")
# tagDF_rel_grps <- readRDS("temp/tagDF_rel_grps_9825.rds")
# tags_comb <- (readRDS("temp/tags_and_obs_comb_ls9825.rds"))$"tags_comb"
tagDF_rel_grps <- readRDS("comp_files/tags_and_obs_comb_ls9825.rds")$"tags_comb"  

gc()

# tag summary table
tagsum2 <- obs_rel_grps2 %>%
  group_by(dat_grp,esutype,reartype,tagid,code) %>%
  mutate(detID_raw=seq_along(mintime)) %>%
  summarize(
    defin_det=unique(detID_raw[init_det]),
    n_defins=sum(det_init),
    n_dets=length(det_init)) 




# obs_rel_grps2 %>% filter(!unassagn)


# eliminates all tags that do not have a detection at a definitive location (e.g., LGR and MCN)
bt=proc.time()
obs_rel_grps3<- obs_rel_grps2 %>% 
  filter(!unassagn) %>% 
  left_join(tagsum2)
proc.time()-bt

# table(obs_rel_grp3$tagid[obs_rel_grps3$dat_grp=="lgr_det"] %in% 
#         obs_rel_grps3$tagid[obs_rel_grps3$dat_grp=="lgr_pooled"])

# creating version of detID at the code level 
#VERY IMPORTANT STEP HERE THAT PREVENTS LGRRRRs from being dropped
obs_rel_grps3$detID_defin[obs_rel_grps3$obssite!="LGRRRR"] <- 
  obs_rel_grps3$detID_raw[obs_rel_grps3$obssite!="LGRRRR"]-obs_rel_grps3$defin_det[obs_rel_grps3$obssite!="LGRRRR"]

# by code summary (codes are combinations of groups and tags)
tagsum3 <- obs_rel_grps3 %>% group_by(code) %>%
  summarize(dets_b4=length(detID_defin<0),
            det_after=length(detID_defin<0))

# ~2.3 million unique tags
length(unique(obs_rel_grps3$tagid))

# ~3.7 million tag histories
table(tagsum2$n_defins==1)

#VERY IMPORTANT STEP HERE
obs_rel_grps3$defin_det_yr[obs_rel_grps3$obssite=="LGRRRR"]
obs_rel_grps3$detID_defin[obs_rel_grps3$obssite=="LGRRRR"]=0 # an index with zero describing the definitive detection event

# Only including the definitive detection and detections that occured afterwards
obs_rel_grps4 <- obs_rel_grps3 %>% 
  filter(defin_det_yr %in% 1998:2025 & detID_defin>=0) %>%
  group_by(code)

prim_obssiteDF <-  readRDS("comp_files/int_recov_sites_ls.rds")$"prim_obssiteDF"
foc_obssites <-c('GRS','B2J','BCC','GOJ','GRJ','JDJ','LMJ','MCJ','ESX','TWX','PD7','PD6','PD8','PD5','PDO','PDW','ICH','LGRRRR')

nrow(obs_rel_grps4)


# failed to include PDO and then noticed that no tagseen at 
# table(obs_rel_grps4$obssite=="PDO",obs_rel_grps4$obssite %in% c('PD7','PD6','PD8','PD5','PDO','PDW'))
# table(obs_rel_grps4$obssite=="PDO")
# table(obs_rel_grps4[obs_rel_grps4$obssite %in% c('PDO','PD7','PD6','PD8','PD5','PDO','PDW'),]$obssite)


length(foc_obssites)
nrow(prim_obssiteDF)
# obs_rel_grps4

##################################################### #
# obs_rel_grps5 
##################################################### #

# nrow(obs_rel_grps5)
# 6304613
obs_rel_grps5 <- obs_rel_grps4 %>%
  filter(obssite %in% foc_obssites) 
# all((obs_rel_grps5 %>% filter(obssite=="PDO") %>% pull(tagid)) %in% (obs_rel_grps5 %>% filter(obssite!="PDO") %>% pull(tagid)))
# nrow(obs_rel_grps5)


# converting LGRRRRs to LGR for pooled tags
obs_rel_grps5$prim_loc_cat[obs_rel_grps5$obssite=="LGRRRR"]="LGR"
obs_rel_grps5 <- obs_rel_grps5[!(obs_rel_grps5$prim_loc_cat=="LGR" & obs_rel_grps5$dat_grp %in% c("mcn_det","mcn_pooled")),]

obs_rel_grps6 <- obs_rel_grps5 %>%  
  filter(as.numeric(det_int_days)<(365/2)) %>% 
  arrange(dat_grp,esutype,defin_det_yr,code,mintime)

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

# ####@@@@@@@@ too early
# # exported to04b
# saveRDS(obs_rel_grps6,"comp_files/obs_rel_grps6_wMCNPOOL.rds")
# ####@@@@@@@@

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
# withn_brkdwn_tab <- obs_rel_grps6 %>% filter(init_det) %>% group_by(dat_grp,esutype,defin_det_yr) %>%
#   summarize(
#     kept=length(which(within)),
#     total=length(within)) %>% 
#   mutate(excluded=total-kept,
#          retained=kept/total)
# saveRDS(withn_brkdwn_tab,"temp/withn_brkdwn_tab.rds")

# summary(withn_brkdwn_tab$excluded)
# between 6 and 500 tags excluded
summary(as.numeric(difftime(obs_rel_grps6$mintime[obs_rel_grps6$before_strt],obs_rel_grps6$defin_det_time_grp[obs_rel_grps6$before_strt],units = "days")))
summary(as.numeric(difftime(obs_rel_grps6$mintime[obs_rel_grps6$after_end],obs_rel_grps6$defin_det_time_grp[obs_rel_grps6$after_end],units = "days")))

# last 1% of tag detections occur up to 2.5 mnths before and  up to 5.5 months after
# difftime(obs_rel_grps6$mintime,obs_rel_grps6$defin_det_time_grp,"days")
gc()

####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# exported to04b
saveRDS(obs_rel_grps6,"comp_files/obs_rel_grps6_wMCNPOOL.rds")
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# filtering down to only those detections that occured within the middle 99%
obs_rel_grps7 <- obs_rel_grps6 %>%
  filter(within,init_det) %>%
  mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"))

####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
saveRDS(obs_rel_grps7,"comp_files/obs_rel_grps7_wMCNPOOL.rds")
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
gc()



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


# reconsider writing a function for this with an argument for whether to use the lgr_pooled by default 
off_used_lgr_det <- off_used %>% filter(dat_grp=="lgr_det")
off_used_lgr_pooled <- off_used %>% filter(dat_grp=="lgr_pooled") %>% ungroup()
off_used_mcn_det <- off_used %>% filter(dat_grp=="mcn_det")
off_used_mcn_pooled <- off_used %>% filter(dat_grp=="mcn_pooled") %>% ungroup()

# off_used_lgr_det$grp_code=paste("lgr_pooled",off_used_lgr_det$esutype,off_used_lgr_det$reartype,off_used_lgr_det$defin_det_yr) 
# off_used_lgr_det <- off_used_lgr_det %>% select(-off_strt,-off_end,-tbtw) %>%
#   left_join(off_used_lgr_pooled %>%
#               select(grp_code,off_strt,off_end,tbtw)
#             ,by="grp_code")

off_used_lgr_det$grp_code=paste("lgr_pooled",off_used_lgr_det$esutype,off_used_lgr_det$reartype,off_used_lgr_det$defin_det_yr)
off_used_lgr_det <- off_used_lgr_det %>% select(-off_strt,-off_end,-tbtw) %>% 
  left_join(off_used_lgr_pooled %>%  
              select(grp_code,off_strt,off_end,tbtw) 
            ,by="grp_code")



# pooling at MCN
off_used_mcn_det$grp_code=paste("mcn_pooled",off_used_mcn_det$esutype,off_used_mcn_det$reartype,off_used_mcn_det$defin_det_yr)
off_used_mcn_det <- off_used_mcn_det %>% select(-off_strt,-off_end,-tbtw) %>% 
  left_join(off_used_mcn_pooled %>%  
              select(grp_code,off_strt,off_end,tbtw) 
            ,by="grp_code")


off_used <- bind_rows(off_used_lgr_det,
                      off_used_lgr_pooled,
                      off_used_mcn_det,
                      off_used_mcn_pooled
                      )

# intended  handle duplicate bins
off_used$grp_code2 <- paste(off_used$dat_grp,off_used$esutype,off_used$reartype,off_used$defin_det_yr)

# off_used$dat_grp,off_used$esutype,off_used$defin_det_yr

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
  
  sub_ls[[ii]] <- obs_rel_grps7[obs_rel_grps7$grp_code==off_used$grp_code2[ii],]
  
  # definitive detection times exposed to bin list
  cat_mat <- sapply(bin_list,function(x){cut(sub_ls[[ii]]$defin_det_time,breaks=x,labels = F)})
  
  if(is.vector(cat_mat)){
    print(ii)
    tmp_df <- data.frame(matrix(cat_mat,nrow=1))
    colnames(tmp_df)=names(cat_mat)
    cat_mat <- tmp_df
  }
  
  
#  # mx_test=cbind(cat_mat,apply(cat_mat,2,function(x){x==max(x)}))
  
  # # special case when cat_mat had only 1 row
  # if(ii %in% c(112,280,424)){
  #   
  #   tmp_df <- data.frame(matrix(cat_mat,nrow=1))
  #   colnames(tmp_df)=names(cat_mat)
  #   
  #   out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
  #                              tmp_df)
  # } else{
    out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
                               cat_mat)
  # }
  
#  # out_ls[[ii]] <- data.frame(sub_ls[[ii]],"strt_time"=off_used$off_strt[ii],"end_time"=off_used$off_end[ii],
#  #                            cat_mat)
  
}
proc.time()-bt # takes ~ 30 seconds

bin_tab_ls_combDF <- do.call(rbind,bin_tab_ls_comb)

head(out_ls)


bt=proc.time()
obs_rel_grps8 <- do.call(rbind,out_ls) #%>% left_join(bin_tab_ls_combDF %>% select(grp_code,bin_end,partial,ind_bin_hours))
proc.time()-bt
# takes ~19 min

table(obs_rel_grps8$dat_grp)

# head(obs_rel_grps8 %>% filter(dat_grp=="mcn_pooled"))
# head(obs_rel_grps8 %>% filter(dat_grp=="mcn_pooled" & relsite=="LGRRRR"))
# head(obs_rel_grps8 %>% filter(dat_grp=="mcn_pooled" & relsite!="LGRRRR"))
# 
# View(obs_rel_grps8)
# nrow(obs_rel_grps8)
# nrow(bin_tab_ls_combDF)
# obs_rel_grps8a <- obs_rel_grps8 %>% left_join(bin_tab_ls_combDF %>% select(grp_code,bin_end,partial,ind_bin_hours))

# verifying that subsetting worked
table(obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="lgr_det"] %in% 
        obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="lgr_pooled"])
# FALSE    TRUE 
# 4312 1189019 

table(obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="mcn_det"] %in% 
        obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="mcn_pooled"])

# gc()
# rm("obs_rel_grps2","obs_rel_grps3","obs_rel_grps4","obs_rel_grps5")
# gc()


####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
saveRDS(obs_rel_grps8,"comp_files/obs_rel_grps8_wMCNPOOL.rds")
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# for a CJS analysis there shouldn't be duplicate detections
# at the same site. This code eliminates all the second detection events
# at the primary locations

bt=proc.time()
obs_rel_grps9 <- obs_rel_grps6 %>%
  mutate(rnd_det_time=lubridate::floor_date(defin_det_time,unit = "12 hours"),
         dup_obs=duplicated(code2)) %>%
  # left_join(obs_rel_grps8 %>% select(dat_grp,esutype,defin_det_time_grp,reartype,tagid,day,days3.5,week1,weeks2,month)) #%>%
  left_join(tagDF_rel_grps %>% select(tagid,AVIAN_recov))
proc.time()-bt 


obs_rel_grps6$defin_det_time_grp
head(data.frame(obs_rel_grps8))


# table(duplicated(obs_rel_grps9$code2))
# table(duplicated(obs_rel_grps10$code2))
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
saveRDS(obs_rel_grps9,"comp_files/obs_rel_grps9_wMCNPOOL.rds")
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

obs_rel_grps10 <- obs_rel_grps9 %>%
  filter(within) %>%
  filter(!dup_obs)

####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
saveRDS(obs_rel_grps10,"comp_files/obs_rel_grps10_wMCNPOOL.rds")
####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# tagDF_rel_grps <- readRDS("comp_files/tags_and_obs_comb_ls9825.rds")$"tags_comb"  
# tagDF_rel_grps <- readRDS("comp_files/tags_and_obs_comb_ls9825.rds")$"tags_comb_inclu"  

tagDF_rel_grps$tagid_inclu <- tagDF_rel_grps$tagid %in% unique(obs_rel_grps10$tagid)
head(tagDF_rel_grps)



# tagDF_rel_grps_inclu <- tagDF_rel_grps$tagid_inclu 
# tagDF_rel_grps
saveRDS(tagDF_rel_grps,"comp_files/tags_comb_inclu_9825_wMCNPOOL.rds")


############################################### #
################ START FOOTER ################# #
############################################### #

table(obs_rel_grps2$tagid[obs_rel_grps2$dat_grp=="lgr_det"] %in% 
        obs_rel_grps2$tagid[obs_rel_grps2$dat_grp=="lgr_pooled"])

table(obs_rel_grps3$tagid[obs_rel_grps3$dat_grp=="lgr_det"] %in% 
        obs_rel_grps3$tagid[obs_rel_grps3$dat_grp=="lgr_pooled"])

table(obs_rel_grps6$tagid[obs_rel_grps6$dat_grp=="lgr_det"] %in% 
        obs_rel_grps6$tagid[obs_rel_grps6$dat_grp=="lgr_pooled"])

# table(obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="lgr_det"] %in% 
#         obs_rel_grps8$tagid[obs_rel_grps8$dat_grp=="lgr_pooled"])

head(data.frame(obs_rel_grps9 %>% filter(dat_grp=="lgr_pooled")))

table(obs_rel_grps9$tagid[obs_rel_grps9$dat_grp=="lgr_det"] %in% 
        obs_rel_grps9$tagid[obs_rel_grps9$dat_grp=="lgr_pooled"])

table(obs_rel_grps9$tagid[obs_rel_grps9$dat_grp=="lgr_det" & obs_rel_grps9$within] %in% 
        obs_rel_grps9$tagid[obs_rel_grps9$dat_grp=="lgr_pooled"& obs_rel_grps9$within])

bin_tab_ls_combDF$bin_code_tmp <- paste(bin_tab_ls_combDF$defin_det_yr,bin_tab_ls_combDF$binID,bin_tab_ls_combDF$bin.1)
table(table(bin_tab_ls_combDF$bin_code_tmp))
bin_tab_ls_combDF$unq_binID

mtchind <- mcn_obs_rel_forDH$day_mtch_code=paste(mcn_obs_rel_forDH$defin_det_yr,mcn_obs_rel_forDH$day,"day")

mtchind <- match(tail(mcn_obs_rel_forDH$day_mtch_code),bin_tab_ls_combDF$bin_code_tmp)
bin_tab_ls_combDF[mtchind,]

tail(data.frame(mcn_obs_rel_forDH))

head(mcn_obs_rel_forDH)
match(paste(mcn_obs_rel_forDH$defin_det_yr,mcn_obs_rel_forDH$day,"day"),bin_tab_ls_combDF$bin_code_tmp)
table(mcn_obs_rel_forDH$dat_grp)


################################################## #
# source("C:/repos/repo_simCJS/simCJS/R/per2_surph_ests.R")
# within_tgs_tb <- obs_rel_grps6 %>% group_by(dat_grp,esutype,tagid) %>% summarize(within=any(within))

# tagDF_rel_grps %>% filter(tagid %in% dup_tagid ) %>% 
#   select(esutype,reartype,tagid) %>% 
#   group_by(esutype,reartype) %>% 
#   summarize(ntags=length(unique(tagid))) 
# 
# obs_rel_grps9 %>% filter(tagid %in% obs_rel_grps9[duplicated(obs_rel_grps9$code2),]$tagid)
# obs_rel_grps9 <- obs_rel_grps9[!duplicated(obs_rel_grps9$code2),]
# 
################################## #
# adding avian recoveries
################################## #

# rel_recov_tgs <- tagDF_rel_grps %>% filter(AVIAN_recov) %>% pull(tagid)

################################################## #
# subsetting tags based on those retained here
################################################## #
# 
# # tags remaining after obs fiiltering
# tagDF_rel_9 <- tagDF_rel_grps %>% filter(tagid %in% unique(obs_rel_grps9$tagid))
# 
# head(tagDF_rel_9)
# # nrow(tagDF_rel_9)
# 
# # could include duplicates
# saveRDS(tagDF_rel_9,"comp_files/tagDF_rel_9.rds") 
# 
################################## #
# mcn_obs_rel_forDH <- mcn_obs_rel %>% filter(!dup_obs)
# lgr_obs_rel_forDH <-  lgr_obs_rel_wdups %>% filter(!dup_obs)

# within the 99th percentile for the definitive detection event
table(obs_rel_grps9$within)

# subsetting MCN det data
# mcn_obs_rel <- obs_rel_grps9 %>% 
#   filter(dat_grp=="mcn_det" & prim_loc_cat %in% c("MCN","BON","JDA","Estuary")) %>%
#   mutate(dup_obs=duplicated(code2))

################################ #
# Detection history
################################ #

# subsetting MCN det data
mcn_obs_rel_wdups <- obs_rel_grps9 %>% 
  filter(dat_grp==c("mcn_pooled","mcn_det") & !(prim_loc_cat %in% c("LGR","LMN","LGS","ICH"))) %>%
  mutate(dup_obs=duplicated(code2))

lgr_obs_rel_wdups <- obs_rel_grps9 %>%
  filter(dat_grp %in% c("lgr_pooled","lgr_det")) %>%
  mutate(dup_obs=duplicated(code2))

######################################## #
#  Exports
######################################## #

mcn_obs_rel_forDH <-  mcn_obs_rel_wdups %>%
  filter(!dup_obs) %>% 
  left_join(tagDF_rel_grps %>% select(tagid,AVIAN_recov)) %>%
  left_join(obs_rel_grps8 %>% select(dat_grp,esutype,reartype,tagid,day,days3.5,week1,weeks2,month))

# mcn_obs_rel_forDH_tmp <- mcn_obs_rel_forDH %>% left_join(obs_rel_grps8 %>% select(dat_grp,esutype,reartype,tagid,day,days3.5,week1,weeks2,month))

lgr_obs_rel_forDH <- lgr_obs_rel_wdups %>% 
  filter(!dup_obs) %>% 
  left_join(tagDF_rel_grps %>% select(tagid,AVIAN_recov)) %>%
  left_join(obs_rel_grps8 %>% select(dat_grp,esutype,reartype,tagid,day,days3.5,week1,weeks2,month))


saveRDS(mcn_obs_rel_forDH,"comp_files/mcn_obs_rel_forDH.rds")
saveRDS(lgr_obs_rel_forDH,"comp_files/lgr_obs_rel_forDH.rds")
