# in_dir="C:/Users/17072/OneDrive - UW/Desktop/historic_trends_leftovers/historic_trends_paper/data"
# out_dir="C:/Users/17072/OneDrive - UW/Desktop/historic_trends_leftovers/historic_trends_paper/comp_data"


in_dir="temp/data_9825"
out_dir="temp/data_9825"
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
    if(y %in% c(1,seq(3,length(fl_ind)))){message(paste(y,"of",length(fl_ind)))}
    tmp <- read.csv(file.path(in_dir,y),row.names = NULL)
    data.frame(fl_nm=y,tmp)
  })
  
  message("\nrbinding by release year")
  csvDF <- do.call(rbind,csv_ls)
  
  message("\nsaving .rds file to 'comp_data'")
  saveRDS(csvDF,file.path(out_dir,paste0(x,"_9825_DF.rds")))
  print(proc.time()-bt)
  
})
proc.time()-bt 

# Sthd: 43 seconds; 117723 kb
# Ch1: 140 seconds; 62426 kb
# Sock: 10 seconds; 10801 kb

# gets rid of a lot of memory
gc()


# C:\repos\CBR_juv_pit\temp\data_9825

###################################################### #
# Putting separate species into a single flat file
###################################################### #

#  takes 1-3 minutes per population
# adds 4gb to RAM
bt=proc.time()
rds_ls <- lapply(spp_codes,function(x) {
  message(x)
  readRDS(file.path(out_dir,paste0(x,"_9825_DF.rds")))} )
proc.time()-bt # 30 seconds

###################################################### #
# Big flat file of detections
###################################################### #

raw_DF <- do.call(rbind,rds_ls)
rm(rds_ls)
gc()

# the full rawDF
head(raw_DF)

table(raw_DF$relbasin)
# Fixing misnamed columns
# rep_nms <- c("fl_nm","tagid","fileid","species","run","reartype","length","broodyr",
#              "migryr","markdate","reltime","relkm","relsite","obssite","sitekm","stage",
#              "mintime","trans_status","trans_date","trans_proj","relbasin","BLANK","relyr","esutype")
# names(raw_DF)=rep_nms

# takes a lot of time
bt=proc.time()
raw_DF$reltime <-  as.POSIXct(raw_DF$reltime ,format="%Y-%m-%d %H:%M:%OS")
raw_DF$mintime <- ifelse(raw_DF$mintime=="",NA,raw_DF$mintime)
raw_DF$mintime <-  as.POSIXct(raw_DF$mintime,format="%Y-%m-%d %H:%M:%OS")
raw_DF$event <- ifelse(is.na(raw_DF$mintime),"release_only","detection")
proc.time()-bt # ~151 seconds minutes

gc()

# extracting unique tagIDs so that I can create a tag-level data set
#  - also add some fields added for later filtering

tags_rel_at_LGR <- unique(subset(raw_DF,relsite=="LGRRRR")$tagid)
trans_tag_obs <- unique(raw_DF[raw_DF$trans_status!="",]$tagid)


bt=proc.time()
tagDF <- raw_DF %>% 
  group_by(tagid,fileid,reartype,length,markdate,relsite,reltime) %>% 
  summarize(
    dets=sum(event=="detection")
    # trans_stat=tagid %in% tags_rel_at_LGR,
    # rel_at_LGR=tagid %in% trans_tag_obs
  ) 
proc.time()-bt # takes X minutes

gc()


tagDF$trans_stat <- !is.na(match(tagDF$tagid,trans_tag_obs))
tagDF$rel_at_LGR <- !is.na(match(tagDF$tagid,tags_rel_at_LGR))


# table(tagDF$trans_stat)

rel_onlyDF <- tagDF[tagDF$dets==0,]

# 56% tagged fish are never redetected
nrow(rel_onlyDF)/nrow(tagDF)

# the number of fish never again detected is a pretty good indicator of the reduction in redetection rates.
# Releases going back to 1996
rel_only_tags <- unique(rel_onlyDF$tagid)
det_tags <- unique(raw_DF$tagid)[(!(unique(raw_DF$tagid) %in%  unique(rel_onlyDF$tagid)))]

# performing check
length(rel_only_tags) + length(det_tags) == length(unique(raw_DF$tagid))

# removing extra stuff
rm(bt)
rm(csv_flnm)
rm(in_dir)
rm(out_dir)
rm(rep_nms)
rm(spp_codes)
rm(rel_onlyDF)

# in_dir="temp/data_9823_SI_test"
# out_dir="temp/data_9823_comp_SI_test"

# old location
save.image("temp/data_comp_WS_9825.Rdata")

# newer location on desktop
# save.image("C:/Users/17072/OneDrive - UW/Desktop/data_comp_WS_23.Rdata")

rm(list=ls())

gc()
