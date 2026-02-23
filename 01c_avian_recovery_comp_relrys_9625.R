# Avian recoveries

####################################################################### #
# Publicly accessible links to PTAGIS reports of recoveries
# 
# Broken into two chunks based on release year
# 
# 1996-2008
# https://ptagisbi.ptagis.org:443/PTAGIS/asp/Main.aspx?evt=4001&src=Main.aspx.4001&visMode=0&reportViewMode=1&reportID=8E9B01D24BF806BE0F30A5B2936C6AF7&Server=NEBULA.PTAGIS.ORG&Project=PTAGIS%20BI&Port=0&connmode=8&ru=1&share=1
# 
# 2009-2025
# https://ptagisbi.ptagis.org:443/PTAGIS/asp/Main.aspx?evt=4001&src=Main.aspx.4001&visMode=0&reportViewMode=1&reportID=AB9DB29943072CB6C37188A5D3DCE8E4&Server=NEBULA.PTAGIS.ORG&Project=PTAGIS%20BI&Port=0&connmode=8&ru=1&share=1
# 
####################################################################### #

av_reco_dir <- "C:/repos/CBR_juv_pit/temp/AVIAN_RECOVS_relyr9625"
av_reco_fls <- file.path(av_reco_dir,dir(av_reco_dir))

library(dplyr)

AV_recov_relyr9625_DF <- do.call(rbind,lapply(av_reco_fls,read.csv))
names(AV_recov_relyr9625_DF) <- c("obssite","migyr","sppcode","Species","reartype","RearType","mort_yr","rel_yr","tagid","reldate","mortdate","relsite","markdate")
# head(tmpDF)


AV_recov_relyr9625_tg <- AV_recov_relyr9625_DF %>% 
  filter(Species %in% c("Chinook","Steelhead","Sockeye")) %>%
  group_by(obssite,tagid,relsite) %>% 
  summarize(nmortrecs=length(mortdate),
            mortdates=paste(unique(mortdate),collapse=","),
            nrelrecs=length(unique(reldate)),
            reldates=paste(unique(reldate),collapse=","))
# table(AV_recov_relyr9625_tg$nrelrecs)
# View(AV_recov_relyr9625_tg)

saveRDS(AV_recov_relyr9625_tg,"comp_files/AV_recov_relyr9625.rds")
# 
# tbl_dup=table(AV_recov_relyr9625_DF$tagid)
# tbl_dup[tbl_dup>1]
# dup_ids <- names(tbl_dup[tbl_dup>1])
# 
# View(AV_recov_relyr9625_DF %>% filter(tagid %in% dup_ids))
