library(dplyr)

headDF <- function(x){head(data.frame(x))}
# saveRDS(list("tags_comb_raw"=tags_comb_raw,"obs_comb_raw"=obs_comb_raw),
#         "temp/tags_and_obs_comb_raw_ls9825.rds")


tags_comb_raw <- (readRDS("temp/tags_and_obs_comb_raw_ls9825.rds"))$"tags_comb_raw"
obs_comb_raw <- (readRDS("temp/tags_and_obs_comb_raw_ls9825.rds"))$"obs_comb_raw"

# table of fish released from 1996-2025
source("C:/repos/CBR_juv_pit/R/get_raw_release_tab.R")
my_rel_tb <- get_raw_release_tab(tags_comb_raw_in = tags_comb_raw)

# table(is.na(tags_comb_raw$min_estMigry))
head(tags_comb_raw$min_estMigry)

# tags_comb_raw %>% group_by(esutype,reartype,) %>% min_estMigry
# 165 not the same
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR==estMigyrMCN) %>% pull())
# 105 cases MCN later year than LGR
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR<estMigyrMCN) %>% pull())
# 55 cases LGR later year than MCN
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR>estMigyrMCN) %>% pull())
# tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR==estMigyrMCN) %>% filter(!same)

# tags_comb_raw <- tags_comb_raw %>% mutate(min_estMigry=ifelse((!is.na(estMigyrLGR) | !is.na(estMigyrMCN)),min(c(estMigyrLGR,estMigyrMCN),na.rm=T),NA))
# tags_comb_raw <- tags_comb_raw %>% rename(min_estMigry=min_est_Migry)
head(tags_comb_raw)

table(is.na(tags_comb_raw$min_estMigry))
# table(is.na(tags_comb_raw$estMigyrLGR))
# table(is.na(tags_comb_raw$estMigyrMCN))

tags_comb_raw$min_estMigry <- NULL

library(dplyr) 
subb_tb1 <- tags_comb_raw %>% 
  filter(!is.na(estMigyrLGR) | !is.na(estMigyrMCN)) # %>%

subb_tb1$min_estMigry <- sapply(1:nrow(offset_relmigr_subb),function(ii) {
  min(offset_relmigr_subb$estMigyrMCN[ii],
      offset_relmigr_subb$estMigyrLGR[ii],na.rm=T)})

my_rel_tb_l <- get_raw_release_tab(tags_comb_raw_in = tags_comb_raw,long=T)

subb_tb2 <- subb_tb1 %>% 
  mutate(yr_diff=paste0("y_",min_estMigry-rel_year)) %>%
  group_by(esutype,reartype,rel_year,yr_diff) %>%
  summarize(ntags=(length(unique(tagid)))) %>%
  tidyr::pivot_wider(values_from = ntags,names_from=yr_diff) %>% 
  left_join(my_rel_tb_l) %>% 
  relocate(esutype,reartype,rel_year,ntags) #%>% 
  # mutate(P_seen=y_0/sum(y_0,y_1,y_2,na.rm=T),
  #        P_y0_cond=y_0/sum(y_0,y_1,y_2,na.rm=T),
  #        P_y1_cond=y_1/sum(y_0,y_1,y_2,na.rm=T),
  #        P_y2_cond=y_2/sum(y_0,y_1,y_2,na.rm=T))
  
# subb_tb2$P_seen <- 
  
t(sapply(1:nrow(subb_tb2),function(ii) {
  c(subb_tb2$y_0[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T),
    subb_tb2$y_1[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T),
    subb_tb2$y_2[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T))
  }))





head(data.frame(subb_tb2))

  # mutate(P_seen=(y_0+y_1+y_2)/ntags)
#,
         P_plus0=y_0/(y_0+y_1+y_2),
         P_plus1=y_1/(y_0+y_1+y_2),
         P_plus2=y_1/(y_0+y_1+y_2))
  

subb_tb2$y_0/subb_tb2$ntags
head(subb_tb2)





tags_comb_raw$min_estMigyr <- NULL

offset_relmigr_subb <- tags_comb_raw %>% 
  filter(!is.na(estMigyrLGR) | !is.na(estMigyrMCN)) %>%
  # mutate()
  mutate(yr_diff=paste(estMigyrLGR-rel_year,estMigyrMCN-rel_year)) %>%
  # mutate(yr_diff=c(estMigyrLGR-rel_year,estMigyrMCN-rel_year)[which(!is.na(c(estMigyrLGR-rel_year,estMigyrMCN-rel_year)))]) %>%
  # mutate(yr_diff=min(c(estMigyrLGR-rel_year,estMigyrMCN-rel_year),na.rm = T)) %>%
  group_by(esutype,reartype,rel_year,yr_diff) %>%
  summarize(ntags=(length(unique(tagid)))) %>%
  tidyr::pivot_wider(values_from = ntags,names_from=yr_diff)


yr_plus0=rowSums(offset_relmigr_subb[,match(c("0 0","0 NA","NA 0",
                                      "1 0","0 1",
                                      "2 0","0 2",
                                      "3 0","0 3")
                                      ,names(offset_relmigr_subb) )],na.rm = T)

yr_plus1=rowSums(offset_relmigr_subb[,match(c("1 1","1 NA","NA 1",
                                     "1 2","2 1",
                                     "1 3","3 1")
                                   ,names(offset_relmigr_subb) )],na.rm = T)

yr_plus2=rowSums(offset_relmigr_subb[,match(c("2 2","2 NA","NA 2",
                                              "3 2")
                                            ,names(offset_relmigr_subb) )],na.rm = T)




rel_det_delayDF <- data.frame(offset_relmigr_subb[,1:3],yr_plus0,yr_plus1,yr_plus2) %>% left_join(my_rel_tb_l) %>% relocate(esutype,reartype,rel_year,ntags)

rel_det_delayDF <- rel_det_delayDF %>% mutate(P_seen=(yr_plus0+yr_plus1+yr_plus2)/ntags,
                           P_plus0=yr_plus0/(yr_plus0+yr_plus1+yr_plus2),
                           P_plus1=yr_plus1/(yr_plus0+yr_plus1+yr_plus2),
                           P_plus2=yr_plus2/(yr_plus0+yr_plus1+yr_plus2))


rel_det_delayDF_plt <- rel_det_delayDF %>%  select(esutype,reartype,rel_year,ntags,P_seen)

library(ggplot2)
ggplot2::ggplot(data=rel_det_delayDF_plt,aes(y=P_seen,x=rel_year,color=reartype)) + facet_grid(reartype~esutype,scales="free_y") + 
  # geom_line() + 
  geom_point() + scale_x_continuous(limits=c(2002,2025))


# release year plot
ggplot2::ggplot(data=rel_det_delayDF_plt,aes(y=ntags,x=rel_year,color=reartype)) + facet_grid(esutype~reartype,scales="free_y") + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(2002,2025))




offset_relmigr_subb$"0 0"+offset_relmigr_subb$"0 NA"+offset_relmigr_subb$"NA 0" + 
  offset_relmigr_subb$"1 0" +offset_relmigr_subb$"0 1"+ 
  offset_relmigr_subb$"2 0" +offset_relmigr_subb$"0 2"+ 
  offset_relmigr_subb$"3 0" +offset_relmigr_subb$"0 3"

offset_relmigr_subb$"1 1"+offset_relmigr_subb$"1 NA"+offset_relmigr_subb$"NA 1" +
  offset_relmigr_subb$"2 1" + offset_relmigr_subb$"1 2"  


# tags_comb_raw$min_est_Migry <- NULL

detyr_LGR <- obs_comb_raw %>% filter(prim_loc_cat=="LGR") %>% 
  group_by(tagid) %>% 
  summarize(mintime=min(mintime)) %>%
  mutate(estMigyrLGR=lubridate::year(mintime))

detyr_MCN <- obs_comb_raw %>% filter(prim_loc_cat=="MCN") %>% 
  group_by(tagid) %>% 
  summarize(mintime=min(mintime)) %>%
  mutate(estMigyrMCN=lubridate::year(mintime))

det_LGR_raw_tb <- tags_comb_raw %>% filter(LGR_loc_det) %>% left_join(detyr_LGR)

det_MCN_raw_tb <- tags_comb_raw %>% filter(MCN_loc_det) %>% left_join(detyr_MCN)
# 
# det_LGR_raw_tb %>% 
#   filter(reartype!="U" & LGR_loc_det & excluded) %>%  
#   group_by(esutype,reartype,LGR_loc_det,rel_year) %>% 
#   summarize(ntags=length(unique(tagid))) %>% 
#   mutate(lb="LGRd") %>%
#   tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,lb))
# LGR_loc_det rel_year SR_Ch1_H_LGRd SR_Ch1_W_LGRd SR_Sock_H_LGRd SR_Sock_W_LGRd SR_Sthd_H_LGRd SR_Sthd_W_LGRd
# <lgl>          <dbl>         <int>         <int>          <int>          <int>          <int>          <int>
#   1 TRUE            1996          1017           297             27             NA            666            371
# 2 TRUE            1997         19182           622             72             NA            922            375
# 3 TRUE            1998         25277          1102            249             16            755            529
# 4 TRUE            1999         13127          1042             38              8            663            529
# 5 TRUE            2000         13803           691              4              7           1184            515
# 6 TRUE            2001         24607          1096             35              1            372            602
# 7 TRUE            2002          7703          2787             42              3             80           1229
# 8 TRUE            2003         14397          5489            330              5            260           2494
# 9 TRUE            2004         27244          9874            722            212            700           4570

det_LGR_raw_tb %>% 
  filter(reartype!="U" & LGR_loc_det & excluded) %>%  
  group_by(esutype,reartype,LGR_loc_det,estMigyr) %>% 
  summarize(ntags=length(unique(tagid))) %>% 
  mutate(lb="LGRd") %>%
  tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,lb))

# release year vs. migration year and 
# example of one esutype/reartype
det_LGR_raw_tb %>% 
  filter(reartype!="U" & LGR_loc_det & excluded) %>%  
  group_by(esutype,reartype,LGR_loc_det,rel_year,estMigyr) %>% 
  summarize(ntags=length(unique(tagid))) %>% 
  mutate(lb="LGRd") %>%
  filter(esutype=="SR_Ch1" & reartype=="W") %>%
  # ungroup() %>%
  tidyr::pivot_wider(values_from = ntags,names_from=c(estMigyr))






head(det_LGR_raw_tb)




get_release_tab(tags_comb_raw_in = tags_comb_raw)

tags_comb_raw$MCN_loc_det
tags_comb_raw$LGR_loc_det

get_raw_DET_LGRRRR_tab(tags_comb_raw_in = tags_comb_raw)

get_EXCLU_raw_DET_LGRRRR_tab(tags_comb_raw_in = tags_comb_raw)

table(obs_comb_raw$stage)

table(obs_comb_raw$obssite,obs_comb_raw$prim_loc_cat)



# # Looking at GRX (i.e., experimental LGR detector from 1995-1999; redundant)
# table(obs_comb_raw$obssite=="GRX")
# 
# obs_comb_raw %>% filter(obssite=="GRX") %>% pull(migryr) %>% table()
# GRX_subb <- obs_comb_raw %>% 
#   filter(obssite %in% c("GRS","GRJ","GRX")) %>% 
#   group_by(obssite,tagid) %>% 
#   summarize(value=length(mintime)) %>% 
#   tidyr::pivot_wider(names_from=obssite) %>%
#   filter(!is.na(GRX)) %>%
#   mutate(same=GRJ==GRX)
# 
# GRX_subb %>% filter(!is.na(GRX))
# table(paste(GRX_subb$GRX,GRX_subb$GRJ))



# all(GRX_subb$`GRJ == GRX`)




head(obs_comb_raw)
head(tags_comb_raw)

source("C:/repos/CBR_juv_pit/R/get_raw_tag_brkdwn.R")

out_tb_ls <- get_raw_tag_brkdwn(tags_comb_raw_in = tags_comb_raw)

tags_comb_raw %>% filter(rel_year==1996) %>% group_by(esutype,reartype,rel_year) %>% summarize(ntags=length(unique(tagid)))
tags_comb_raw %>% filter(rel_year==1996) %>% group_by(esutype,reartype,lgr_rel ,rel_year) %>% summarize(ntags=length(unique(tagid)))
tags_comb_raw %>% filter(rel_year==1996 ) %>% group_by(esutype,rel_year,rel_at_LGR) %>% summarize(ntags=length(unique(tagid)))
tags_comb_raw %>% filter(rel_year==1996 & !(nonLGR_intra_dam_rel | trans_statTF)) %>% group_by(esutype,reartype,rel_year,rel_at_LGR) %>% summarize(ntags=length(unique(tagid)))
tags_comb_raw %>% filter(rel_year==1996 ) %>% group_by(esutype,reartype,rel_year,rel_at_LGR) %>% summarize(ntags=length(unique(tagid)))

# tags_comb_raw$nonLGR_intra_dam_rel

out_tb_ls$summ_tb_tmp_w2

out_tb_ls$summ_tb_tmp3# %>% filter()
out_tb_ls$summ_tb_tmp4# %>% filter()
out_tb_ls$summ_tb_tmp5# %>% filter()


out_tb_ls$summ_tb_tmp4

out_tb_ls$summ_tb_tmp5

obs_comb_raw %>% filter(relsite=="LGRRRR")





table(out_tb_ls$sfilter()table(out_tb_ls$summ_tb_tmp4$reartype)

supptb_rel_det_tab_w <-  out_tb_ls$summ_tb_tmp4 %>%   
  filter(reartype!="U") %>%
  mutate(reartype=factor(reartype,c("W","H"))) %>%
  select(esutype,reartype,rel_year,tot_rel,rel_at_LGR,seen_post_rel_FOC,LGR_loc_det,MCN_loc_det) %>%
  rename(R_tot=tot_rel,R_LGR=rel_at_LGR,Det=seen_post_rel_FOC,Det_LGR=LGR_loc_det,Det_MCN=MCN_loc_det) %>% 
  tidyr::pivot_longer(cols=4:8) %>% tidyr::pivot_wider(names_from=c(reartype,name)) %>%
  mutate(COMB_R_tot=H_R_tot+W_R_tot) %>% relocate(esutype,rel_year,COMB_R_tot)

unique(supptb_rel_det_tab_w$esutype)
esut_v <- c("SR_Ch1","SR_Sock","SR_Sthd")

supptb_rel_ls <- lapply(esut_v,function(ii) supptb_rel_det_tab_w %>% filter(esutype==ii) %>% ungroup() %>% select(-esutype))
names(supptb_rel_ls) <- supptb_rel_ls

dir.create("tables/reltb",recursive = T)
lapply(1:length(esut_v),function(ii) write.csv(supptb_rel_ls[[ii]],paste0("tables/reltb/supptb_rel_",esut_v[ii],".csv"),row.names = F) )






out_tb_ls$summ_tb_tmp4 %>%   
  filter(reartype!="U") %>% mutate(reartype=factor(reartype,c("W","H"))) %>%
  select(esutype,reartype,rel_year,tot_rel,rel_at_LGR,seen_post_rel_FOC,seen_post_rel_FOC_exclu,LGR_loc_det,MCN_loc_det) %>%
  rename(R_tot=tot_rel,R_LGR=rel_at_LGR,Det=seen_post_rel_FOC,Exclu=seen_post_rel_FOC_exclu,Det_LGR=LGR_loc_det,Det_MCN=MCN_loc_det) %>% 
  tidyr::pivot_longer(cols=4:9) %>% tidyr::pivot_wider(names_from=c(name,reartype)) %>%
  mutate(R_tot_COMB=R_tot_H+R_tot_W) %>% relocate(esutype,rel_year,R_tot_COMB)


# tags_comb_raw$nonLGR_intra_dam_rel
# tags_comb_raw$trans_statTF

table(tags_comb_raw$trans_statTF,
      tags_comb_raw$nonLGR_intra_dam_rel)


out_tb_ls$summ_tb_tmp3# %>% filter()
out_tb_ls$summ_tb_tmp4# %>% filter()




out_tb_ls$summ_tb_tmp3 %>% filter(rel_year==2025)
# before revision
# esutype reartype rel_year tot_rel seen_post_rel seen_post_rel_FOC det_post_rel recov_post_rel only_recov_post_rel rel_at_LGR LGR_loc_det MCN_loc_det
# <chr>   <chr>       <dbl>   <int>         <int>             <int>        <int>          <int>               <int>      <int>       <int>       <int>
# 1 SR_Ch1  H          2025  192065         59075             28822        28640            284                 182          0       17677        2782
# 2 SR_Ch1  W            2025   73708         21372              6020         5998             37                  22       7174        3687         617
# 3 SR_Sock H            2025   58540         31754             12381        12348             47                  33          1       10998         732
# 4 SR_Sock W            2025     102             2                 2            2              0                   0          0           1           0
# 5 SR_Sthd H            2025   68699         43351             26398        26297            280                 101          0       22633         493
# 6 SR_Sthd W            2025   44364         13300              7691         7628            132                  63       7657        6008         147
# 
# After PDO and PDW
# look at * in col "seen_post_rel_FOC"
# esutype reartype rel_year tot_rel seen_post_rel seen_post_rel_FOC det_post_rel recov_post_rel only_recov_post_rel rel_at_LGR LGR_loc_det MCN_loc_det
# <chr>   <chr>       <dbl>   <int>         <int>             <int>        <int>          <int>               <int>      <int>       <int>       <int>
# 1 SR_Ch1  H          2025  192065         59075             29016*        28834            284                 182          0       17677        2782
# 2 SR_Ch1  W            2025   73708         21372              6048*         6026             37                  22       7174        3687         617
# 3 SR_Sock H            2025   58540         31754             12384*        12352             47                  32          1       10998         732
# 4 SR_Sock W            2025     102             2                 2            2              0                   0          0           1           0
# 5 SR_Sthd H            2025   68699         43351             26428*        26327            280                 101          0       22633         493
# 6 SR_Sthd W            2025   44364         13300              7716*         7653            132                  63       7657        6008         147






# LOOKING AT TAGS THAT WERE DETECTED BUT WHICH WE DO NOT RECOGNIZE
#
# (tags_comb_raw)
# slipped_by_tgs <- tags_comb_raw %>% filter(dets>0 & !(AVIAN_recov | prim_loc_det)) %>% pull(tagid)
# INT_sites <- foreign::read.dbf("temp/PTAGIS_INT_Sites.dbf")
# slipped_by_obsDF <-obs_comb_raw %>% filter(tagid %in% slipped_by_tgs)
# slipped_by_obssites <- unique(slipped_by_obsDF$obssite)


# sort(table(slipped_by_obsDF$obssite))
# dftmp <-data.frame(table(slipped_by_obsDF$obssite))
# names(dftmp) <- c("SiteCode","freq")
# View(dftmp %>% left_join(INT_sites) %>% arrange(-freq))
INT_sites %>% filter(SiteCode %in% c(slipped_by_obssites)) 

nrow(tags_comb_raw %>% filter(dets>0 & !prim_loc_det))
nrow(tags_comb_raw)

gc()

out_tb_ls <- get_raw_tag_brkdwn(tags_comb_raw_in = tags_comb_raw)
out_tb_ls$summ_tb_tmp3


out_tb_ls$summ_tb_tmp1
out_tb_ls$summ_tb_tmp_w1

out_tb_ls$summ_tb_tmp1
out_tb_ls$summ_tb_tmp2

tags_comb_raw$at_LGR


tags_comb_raw %>% 
  group_by(esutype,reartype,rel_year,event,rel_at_LGR,trans_stat,AVIAN_recov) %>% 
  summarize(ntags=length(unique(tagid)))

tags_comb_raw




headDF(tags_comb_raw)

# tags_comb_raw$rel_year
# gc()
