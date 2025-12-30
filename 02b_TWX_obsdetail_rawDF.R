# in_dir="C:/Users/17072/OneDrive - UW/Desktop/historic_trends_leftovers/historic_trends_paper/data"
# out_dir="C:/Users/17072/OneDrive - UW/Desktop/historic_trends_leftovers/historic_trends_paper/comp_data"


in_dir="temp/data_9825_TWX"
out_dir="temp/data_9825_TWX"
# if(!dir.exists(in_dir)){dir.create(out_dir,recursive = T)}

# dir.create(out_dir)

# in_out_dir="temp/data_9823_SI_test"

library(dplyr)
library(tidyr)

bt=proc.time()
csv_flnm <- dir(in_dir,pattern = ".csv")
spp_codes= c("Sthd","Ch1","Sock")

# confirm that rep_nms values correspond with column types
bt=proc.time()
sapply(spp_codes,function(x){
  fl_ind <- grep(csv_flnm,pattern=x) 
  
  message(x)
  message("\nloading .csvs")
  csv_ls <-lapply(csv_flnm[fl_ind],function(y) {
    if(y %in% c(1,seq(3,27))){message(paste(y,"of",length(fl_ind)))}
    tmp <- read.csv(file.path(in_dir,y),row.names = NULL)
    data.frame(fl_nm=y,tmp)
  })
  
  message("\nrbinding by release year")
  csvDF <- do.call(rbind,csv_ls)
  
  message("\nsaving .rds file to 'comp_data'")
  saveRDS(csvDF,file.path(out_dir,paste0(x,"_9824_DF.rds")))
  print(proc.time()-bt)
  
})
proc.time()-bt #only few seconds

#  takes 1-3 minutes per population
# adds 4gb to RAM

bt=proc.time()
TWX_rds_ls <- lapply(spp_codes,function(x) {
  message(x)
  readRDS(file.path(out_dir,paste0(x,"_9824_DF.rds")))} )
proc.time()-bt # 1.5 minutes

raw_TWX_DF <- do.call(rbind,TWX_rds_ls)



rm(rds_ls)
gc()

# the full rawDF
head(raw_TWX_DF)

table(raw_TWX_DF$relbasin)

# Fixing misnamed columns (not sure if still relevant)
# rep_nms <- c("fl_nm","tagid","fileid","species","run","reartype","length","broodyr",
#              "migryr","markdate","reltime","relkm","relsite","obssite","sitekm","stage",
#              "mintime","trans_status","trans_date","trans_proj","relbasin","BLANK","relyr","esutype")
# names(raw_TWX_DF)=rep_nms

# quick
bt=proc.time()
raw_TWX_DF$reltime <-  as.POSIXct(raw_TWX_DF$reltime ,format="%Y-%m-%d %H:%M:%OS")
raw_TWX_DF$mintime <- ifelse(raw_TWX_DF$mintime=="",NA,raw_TWX_DF$mintime)
raw_TWX_DF$mintime <-  as.POSIXct(raw_TWX_DF$mintime,format="%Y-%m-%d %H:%M:%OS")
raw_TWX_DF$event <- ifelse(is.na(raw_TWX_DF$mintime),"release_only","detection")
proc.time()-bt # ~5 minutes

gc()

# creating tag-level data set with some fields added for later filtering
tags_rel_at_LGR <- unique(subset(raw_TWX_DF,relsite=="LGRRRR")$tagid)
trans_tag_obs <- unique(raw_TWX_DF[raw_TWX_DF$trans_status!="",]$tagid)


bt=proc.time()
TWX_tagDF <- raw_TWX_DF %>% 
  group_by(tagid,fileid,reartype,length,markdate,relsite,reltime) %>% 
  summarize(
    dets=sum(event=="detection")
    # trans_stat=tagid %in% tags_rel_at_LGR,
    # rel_at_LGR=tagid %in% trans_tag_obs
  ) 
proc.time()-bt # takes X minutes

gc()


# tagDF$trans_stat <- !is.na(match(tagDF$tagid,trans_tag_obs))
# tagDF$rel_at_LGR <- !is.na(match(tagDF$tagid,tags_rel_at_LGR))
# 
# # table(tagDF$trans_stat)
# 
# rel_onlyDF <- tagDF[tagDF$dets==0,]
# 
# 
# # the number of fish never again detected is a pretty good indicator of the reduction in redetection rates.
# # Releases going back to 1996
# rel_only_tags <- unique(rel_onlyDF$tagid)
# det_tags <- unique(raw_TWX_DF$tagid)[(!(unique(raw_TWX_DF$tagid) %in%  unique(rel_onlyDF$tagid)))]
# 
# # performing check
# length(rel_only_tags) + length(det_tags) == length(unique(raw_TWX_DF$tagid))


# out_dir="comp_files/data_9825_TWX"
# raw_TWX_DF <- readRDS("temp/raw_TWX_DF.rds")

saveRDS(raw_TWX_DF,file.path(out_dir,"raw_TWX_DF_9825.rds"))




# removing extra stuff
# rm(bt)
# rm(csv_flnm)
# rm(in_dir)
# rm(out_dir)
# rm(rep_nms)
# rm(spp_codes)
# rm(rel_onlyDF)


# in_dir="temp/data_9823_SI_test"
# out_dir="temp/data_9823_comp_SI_test"

# old location didn't use
# save.image("temp/data_9823_comp_SI_test_TWX/data_comp_WS_9823_TWX.Rdata")

# newer location on desktop
# save.image("C:/Users/17072/OneDrive - UW/Desktop/data_comp_WS_23.Rdata")

rm(list=ls())

gc()
