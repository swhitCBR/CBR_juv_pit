library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)


mytheme=theme(axis.line = element_line(colour = "black"),
              axis.title=element_text(colour = "black",size = 18),
              axis.text = element_text(colour = "black",size=12),
              axis.ticks = element_line(colour = 'black'),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border =  element_rect(fill=NA,colour = "black"), #element_blank(),
              panel.background = element_blank(),
              plot.margin = unit(c(1,1,1,1), "cm"),
              strip.text.x = element_text(size = 16),
              strip.text.y = element_text(size = 16),
              strip.background = element_blank(),
              axis.title.y=element_text(margin=margin(0,20,0,0)),
              axis.title.x=element_text(margin=margin(20,0,0,0)),
              legend.key = element_blank(),
              legend.title = element_text(size=14),
              legend.text =element_text(size=12),
              plot.title = element_text(hjust = 0,size=18))


# source("functions.R")

S_EST_THRESH = 1.2
S_CV_EST_THRESH = 50


lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")

head(lgr_dh_tab2)

############################## #
# Separate reartypes
############################## #

# 140 scenarios
lgr_grp_summ_tab <- lgr_dh_tab2 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code) %>%
  summarize(days=length(unique(day)),
            days3.5=length(unique(days3.5)),
            week1 =length(unique(week1)),
            weeks2 =length(unique(weeks2)),
            month=length(unique(month)),
            years=length(unique(defin_det_yr))) %>%
  mutate(n_datasets=sum(days,days3.5,week1,weeks2,month,years))

lgr_grp_summ_tab2 <-  lgr_grp_summ_tab %>% group_by(dat_grp,reartype,esutype) %>%
  summarize(
    day_rng=paste(min(days),max_days=max(days),sep="-"),
    avg_days=mean(days),
    n_days=sum(days),
    n_weeks=sum(week1),
    n_months=sum(month),
    n_datasets=sum(n_datasets)) %>% arrange(dat_grp)


lgr_grp_summ_tab3 <-  lgr_grp_summ_tab %>% group_by(dat_grp,esutype) %>%
  summarize(
    day_rng=paste(min(days),max_days=max(days),sep="-"),
    avg_days=mean(days),
    n_days=sum(days),
    n_weeks=sum(week1),
    n_months=sum(month),
    n_datasets=sum(n_datasets)) %>% arrange(dat_grp)

lgr_grp_summ_tab
lgr_grp_summ_tab2
lgr_grp_summ_tab3

################################ #

# tag-level table
lgr_dh_tab2$DH_red <- factor(paste(lgr_dh_tab2$MCN,lgr_dh_tab2$BON),levels=c("1 1","1 0","0 1","0 0"))

bt=proc.time()
out_ls <- list(
  "yr_grp_tab"=data.frame(aggre_lev="year",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,DH_red) %>%
                            summarize(n_tags=length(unique(code))) %>%
                            pivot_wider(names_from=DH_red,values_from = n_tags)) %>%replace(is.na(.), 0),
  
  "mnth_grp_tab"=data.frame(aggre_lev="month",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,month,DH_red) %>%
                              summarize(n_tags=length(unique(code))) %>%
                              pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "2week_grp_tab"=data.frame(aggre_lev="2weeks",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,weeks2,DH_red) %>%
                               summarize(n_tags=length(unique(code))) %>%
                               pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "week_grp_tab"=data.frame(aggre_lev="week",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,week1,DH_red) %>%
                              summarize(n_tags=length(unique(code))) %>%
                              pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "3.5day_grp_tab"=data.frame(aggre_lev="halfweek",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,days3.5,DH_red) %>%
                                summarize(n_tags=length(unique(code))) %>%
                                pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "day_grp_tab"=data.frame(aggre_lev="day",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,day,DH_red) %>%
                             summarize(n_tags=length(unique(code))) %>%
                             pivot_wider(names_from=DH_red,values_from = n_tags)) %>%replace(is.na(.), 0))
proc.time()-bt



source("R/CJS_aggre_est_2per.R")
source("R/CJS_aggre_fxns.R")
source("R/get_CJS_count_ls.R")
source("R/get_mom_mat.R")


# batch_run mom estimates
out_ls_sepBON <- lapply(out_ls,get_mom_mat)
dat_sepDFBON <- bind_rows(out_ls_sepBON) %>% mutate(RT_comb=FALSE) %>% relocate(RT_comb)
# dat_combDF <- bind_rows(out_ls_RTcomb) %>% mutate(RT_comb=TRUE) %>% relocate(RT_comb)
# 

# saveRDS(dat_sepDFBON,"temp/dat_sepDFBON.rds")




#################### combined bonneville and estuary
lgr_dh_tab2$BONEST <- as.numeric(lgr_dh_tab2$BON==1 | lgr_dh_tab2$Estuary==1)


table(lgr_dh_tab2$BONEST)
table(lgr_dh_tab2$BON)
table(lgr_dh_tab2$Estuary)

lgr_dh_tab2$DH_red <- factor(paste(lgr_dh_tab2$MCN,lgr_dh_tab2$BONEST),levels=c("1 1","1 0","0 1","0 0"))

bt=proc.time()
out_lsBONEST <- list(
  "yr_grp_tab"=data.frame(aggre_lev="year",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,DH_red) %>%
                            summarize(n_tags=length(unique(code))) %>%
                            pivot_wider(names_from=DH_red,values_from = n_tags)) %>%replace(is.na(.), 0),
  
  "mnth_grp_tab"=data.frame(aggre_lev="month",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,month,DH_red) %>%
                              summarize(n_tags=length(unique(code))) %>%
                              pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "2week_grp_tab"=data.frame(aggre_lev="2weeks",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,weeks2,DH_red) %>%
                               summarize(n_tags=length(unique(code))) %>%
                               pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "week_grp_tab"=data.frame(aggre_lev="week",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,week1,DH_red) %>%
                              summarize(n_tags=length(unique(code))) %>%
                              pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "3.5day_grp_tab"=data.frame(aggre_lev="halfweek",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,days3.5,DH_red) %>%
                                summarize(n_tags=length(unique(code))) %>%
                                pivot_wider(names_from=DH_red,values_from = n_tags)) %>% replace(is.na(.), 0),
  
  "day_grp_tab"=data.frame(aggre_lev="day",lgr_dh_tab2 %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,day,DH_red) %>%
                             summarize(n_tags=length(unique(code))) %>%
                             pivot_wider(names_from=DH_red,values_from = n_tags)) %>%replace(is.na(.), 0))
proc.time()-bt



# batch_run mom estimates
out_ls_sepBON_EST <- lapply(out_lsBONEST,get_mom_mat)
head(out_ls$yr_grp_tab)

dat_sepDF_BON_EST <- bind_rows(out_ls_sepBON_EST) %>% mutate(RT_comb=FALSE) %>% relocate(RT_comb)

# saveRDS(dat_sepDF_BON_EST,"temp/dat_sepDFBON_EST.rds")

# saveRDS(lgr_mcn_sep_out_DF,
#         "temp/lgr_mcn_sepBONEST_DF.rds")

# lgr_code_summ_tabs=list(
#   "lgr_grp_summ_tab"=lgr_grp_summ_tab,
#   "lgr_grp_summ_tab2"=lgr_grp_summ_tab2,
#   "lgr_grp_summ_tab3"=lgr_grp_summ_tab3)
# saveRDS(lgr_code_summ_tabs,"temp/lgr_code_summ_tabs.rds")
# 

dat_sepDFBON_EST <- bind_rows(out_ls_sepBON_EST) %>% mutate(RT_comb=FALSE) %>% relocate(RT_comb)
# dat_combDF <- bind_rows(out_ls_RTcomb) %>% mutate(RT_comb=TRUE) %>% relocate(RT_comb)
# 



# dat_sepDFBON <- readRDS("temp/dat_sepDFBON.rds")
# readRDS("temp/dat_sepDFBON_EST.rds")
bin_tab_ls_combDF <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

# bin_tab_ls_combDF <- readRDS("temp/bin_tab_ls_combDF.rds")
# head(bin_tab_ls_combDF)


lgr_est_outDF_RAW <- dat_sepDF_BON_EST


lgr_est_outDF <- lgr_est_outDF_RAW# %>% filter(aggre_lev!="year")

mtch_key=data.frame(
  aggre_lev=c("day","halfweek","week","2weeks","month","year"),
  bin=c("day","days3.5","week1","weeks2","month","year"))

bin_tab_ls_combDF$aggre_lev <- mtch_key$aggre_lev[match(bin_tab_ls_combDF$bin,mtch_key$bin)]
bin_tab_ls_combDF$partial=bin_tab_ls_combDF$partial
lgr_est_outDF$bin <- mtch_key$bin[match(lgr_est_outDF$aggre_lev,mtch_key$aggre_lev)]

bin_grps_DF <- bin_tab_ls_combDF %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,aggre_lev,grp_code) %>%
  mutate(bad_last_bin=any(partial)) 

bad_bin_grps <- bin_grps_DF %>%
  filter(bad_last_bin) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,aggre_lev,grp_code,bad_last_bin) %>%
  summarize(hours_in_bad_bin=ind_bin_hours[which(partial)]) 

bad_bin_grps


lgr_est_outDF$year=lgr_est_outDF$defin_det_yr
lgr_est_outDF$col_ind <- match(lgr_est_outDF$bin,names(lgr_est_outDF))
lgr_est_outDF$bin_unit <- as.numeric(sapply(1:length(lgr_est_outDF$col_ind),
                                            function(x){lgr_est_outDF[x,lgr_est_outDF$col_ind[x]]}))
lgr_est_outDF$code3 <- paste(lgr_est_outDF$grp_code,lgr_est_outDF$bin_unit)


lgr_est_outDF1 <- lgr_est_outDF %>%  group_by(dat_grp,esutype,reartype,defin_det_yr,aggre_lev,grp_code) %>%
  mutate(last_bin=max(bin_unit),
         is_last=last_bin==bin_unit) %>%
  relocate(code3,bin_unit,last_bin,is_last)

# head(data.frame(lgr_est_outDF1))
# head(data.frame(lgr_est_outDF1[is.na(lgr_est_outDF1$last_bin),]))


tt= lgr_est_outDF1 %>% 
  left_join(bad_bin_grps)
head(data.frame(tt[is.na(tt$bad_last_bin),]))


no_prob_bin_DF <- bin_grps_DF %>% filter(!bad_last_bin) %>% 
  group_by(dat_grp,esutype,defin_det_yr,reartype,grp_code,bin,aggre_lev) %>% 
  mutate(code4=paste(aggre_lev,grp_code)) 

no_prob_codes <- no_prob_bin_DF  %>% pull(code4)

lgr_est_outDF1 <- lgr_est_outDF1 %>% mutate(code4=paste(aggre_lev,grp_code)) 

table(no_prob_codes %in% lgr_est_outDF1$code4)

lgr_est_outDF1$no_prob_codes <- lgr_est_outDF1$code4 %in% no_prob_codes
lgr_est_outDF1$pot_bin_hours <- c(24,24*3.5,24*7,24*14,24*28)[match(lgr_est_outDF1$aggre_lev,c("day","halfweek","week","2weeks","month"))]
lgr_est_outDF2 <- lgr_est_outDF1 %>% 
  left_join(bad_bin_grps) %>%
  mutate(PARTIAL_BIN=bad_last_bin & is_last,
         hours_in_bin=ifelse(PARTIAL_BIN,hours_in_bad_bin,pot_bin_hours),
         days_in_bad_bin=hours_in_bad_bin/24,
         total_days_in_bad_bin=pot_bin_hours/24) %>%
  relocate(PARTIAL_BIN,hours_in_bad_bin,pot_bin_hours,days_in_bad_bin,total_days_in_bad_bin) %>%
  mutate(
    above11=s1 > S_EST_THRESH,
    s1SE=sqrt(s1_var),
    s1CV=(s1SE/s1)*100,
    abvCVthresh=s1CV>S_CV_EST_THRESH,
    no_estimate=(is.na(s1) | is.infinite(s1)),
    partial_prop=hours_in_bin/pot_bin_hours) %>% select(-month,-weeks2,-week1,-days3.5,-day)

head(data.frame(lgr_est_outDF2))
head(data.frame(lgr_est_outDF2 %>% filter(is.na(PARTIAL_BIN)) %>% select(grp_code,aggre_lev,no_prob_codes)))

# can ignore NAs
all(lgr_est_outDF2 %>% filter(is.na(PARTIAL_BIN)) %>% pull(no_prob_codes))
# no_prob_bin_DF$ind_bin_hours

# correcting groups which didn't have any extra bins
lgr_est_outDF2$PARTIAL_BIN[is.na(lgr_est_outDF2$PARTIAL_BIN)]=FALSE
lgr_est_outDF2[is.na(lgr_est_outDF2$PARTIAL_BIN),]$hours_in_bin=lgr_est_outDF2[is.na(lgr_est_outDF2$PARTIAL_BIN),]$pot_bin_hours
lgr_est_outDF2[is.na(lgr_est_outDF2$hours_in_bin),]$hours_in_bin=lgr_est_outDF2[is.na(lgr_est_outDF2$hours_in_bin),]$pot_bin_hours

lgr_est_outDF2$days_in_bin <- lgr_est_outDF2$hours_in_bin/24

# classifying BAD
lgr_est_outDF2$BAD=lgr_est_outDF2$no_estimate | lgr_est_outDF2$above11 | lgr_est_outDF2$abvCVthresh
lgr_est_outDF2$GOOD=!(lgr_est_outDF2$BAD)


lgr_est_outDF2 <- lgr_est_outDF2 %>% mutate(inadmiss_either= !no_estimate & (above11 | abvCVthresh))
# lgr_est_outDF2 <- lgr_est_outDF2 %>% mutate(inadmiss_either=sum( !no_estimate & (above11 | abvCVthresh),na.rm=T))

# lgr_est_outDF2

# days_in_bin

########################### #
# no estimates general
########################### #

tmp_w_partial_w <- lgr_est_outDF2 %>%  group_by(dat_grp,esutype,RT_comb,reartype,aggre_lev,defin_det_yr,grp_code) %>%
  summarize(
    tot_dib=sum(days_in_bin),
    tot_pot_dib=sum(pot_bin_hours/24),
    GOOD_dib=sum(days_in_bin[GOOD]),
    BAD_dib=sum(days_in_bin[BAD]),
    n_partial_bins=sum(PARTIAL_BIN),
    estimable=sum(!no_estimate),#,na.rm=T),
    nonestimable=sum(no_estimate),#,na.rm=T),
    total=length(no_estimate),
    prop_estim=estimable/total,
    too_big=sum(above11,na.rm=T),
    above_CV_thresh=sum(abvCVthresh,na.rm=T),
    inadmiss_either=sum( !no_estimate & (above11 | abvCVthresh),na.rm=T),
    inadmiss_both=sum( !no_estimate & (above11 & abvCVthresh),na.rm=T),
    inadmiss_CV=sum( !no_estimate & above11 & (!abvCVthresh),na.rm=T),
    inadmiss_BIG=sum( !no_estimate & (!above11)  & abvCVthresh,na.rm=T),
    BAD=sum(BAD,na.rm=T)) %>% mutate(GOOD=total-BAD)

tmp_w_partial_w$BAD+tmp_w_partial_w$GOOD==tmp_w_partial_w$total

tmp_wo_partial_w <- lgr_est_outDF2 %>%  
  group_by(dat_grp,esutype,RT_comb,reartype,aggre_lev,defin_det_yr,grp_code) %>%
  filter(!PARTIAL_BIN) %>%
  summarize(
    tot_dib=sum(days_in_bin),
    tot_pot_dib=sum(pot_bin_hours/24),
    GOOD_dib=sum(days_in_bin[GOOD]),
    BAD_dib=sum(days_in_bin[BAD]),
    n_partial_bins=sum(PARTIAL_BIN),
    estimable=sum(!no_estimate),#,na.rm=T),
    nonestimable=sum(no_estimate),#,na.rm=T),
    total=length(no_estimate),
    prop_estim=estimable/total,
    too_big=sum(above11,na.rm=T),
    above_CV_thresh=sum(abvCVthresh,na.rm=T),
    inadmiss_either=sum( !no_estimate & (above11 | abvCVthresh),na.rm=T),
    inadmiss_both=sum( !no_estimate & (above11 & abvCVthresh),na.rm=T),
    inadmiss_CV=sum( !no_estimate & above11 & (!abvCVthresh),na.rm=T),
    inadmiss_BIG=sum( !no_estimate & (!above11)  & abvCVthresh,na.rm=T),
    BAD=sum(BAD,na.rm=T)) %>% mutate(GOOD=total-BAD)


tmp_cat_partial_w <-  bind_rows(
  data.frame(grp="without partial",tmp_wo_partial_w),
  data.frame(grp="with partial",tmp_w_partial_w))

# tmp_cat_partial_w$BAD+
#   tmp_cat_partial_w$GOOD==tmp_cat_partial_w$total


tmp_wo_partial_l <- melt(tmp_wo_partial_w, #%>% 
                         # select(dat_grp,esutype,RT_comb,reartype,aggre_lev,defin_det_yr,grp_code,estimable,too_big,above_CV_thresh),
                         variable.name = "count",value.name = "n_datasets")

tmp_w_partial_l <- melt(tmp_w_partial_w, #%>% 
                        # select(dat_grp,esutype,RT_comb,reartype,aggre_lev,defin_det_yr,grp_code,estimable,too_big,above_CV_thresh),
                        variable.name = "count",value.name = "n_datasets")


tmp_cat_partial_l <-  rbind(
  data.frame(grp="without partial",tmp_wo_partial_l),
  data.frame(grp="with partial",tmp_w_partial_l))


tmp_cat_partial_l$aggre_lev=factor(tmp_cat_partial_l$aggre_lev,levels=c("day","halfweek","week","2weeks","month","year"))
lab_key=data.frame(count=c("estimable","too_big","above_CV_thresh"),
                   new=c("n_estimable","s1 > 1.1","CV > 20%"))
# 
# table(tmp_cat_partial_l$count=="BAD")
# table(tmp_cat_partial_l$count)


################################################# #
# By n_datasets breakdown
################################################# #

tmp_cat_partial_l4plt <- tmp_cat_partial_l %>% 
  filter(count %in% c("GOOD","BAD"))


# tmp_cat_partial_l4plt <- tmp_cat_partial_l %>% 
#   filter(count %in% c("inadmiss_CV","inadmiss_BIG","GOOD","nonestimable"))

tmp_cat_partial_l4plt <- tmp_cat_partial_l %>%
  filter(aggre_lev %in% c("day","halfweek","week","month") &
           count %in% c("nonestimable","inadmiss_either","estimable"))


gp_lgr_BON_n_datasets <- ggplot() + 
  geom_bar(data=tmp_cat_partial_l4plt %>% filter(grp=="without partial"),# & !PARTIAL_BIN),
           aes(x=as.numeric(defin_det_yr),y=n_datasets,fill=count),
           stat="identity",position="stack",color="black")+
  scale_x_continuous(breaks=seq(1998,2023,3)) +
  scale_fill_manual(values=c("green3","orange3","darkred")) +
  facet_grid(aggre_lev~esutype+reartype,scales = "free") +ggtitle("without partial")+ mytheme +
  theme(legend.position = "bottom") + labs(y="Temporal Bins",x="Year") +geom_vline(xintercept = 2019.5) 



################################################# #
# By Days with estimates
################################################# #


DIB_tabw <- lgr_est_outDF2 %>%  
  group_by(dat_grp,esutype,RT_comb,reartype,defin_det_yr,grp_code,aggre_lev,bin_unit,PARTIAL_BIN,partial_prop) %>%
  summarize(
    tot_dib=sum(days_in_bin),
    tot_pot_dib=sum(pot_bin_hours/24),
    GOOD_dib=sum(days_in_bin[GOOD]),
    NONESTIM_dib=sum(days_in_bin[no_estimate]),
    INADMISS_dib=sum(days_in_bin[inadmiss_either]),
    BAD_dib=sum(days_in_bin[BAD]))  %>%
  group_by(dat_grp,esutype,RT_comb,reartype,defin_det_yr,grp_code,aggre_lev) %>%
  summarize(
    tot_dib=sum(tot_dib),
    tot_pot_dib=sum(tot_pot_dib),
    GOOD_dib=sum(GOOD_dib),
    NONESTIM_dib=sum(NONESTIM_dib),
    INADMISS_dib=sum(INADMISS_dib),
    BAD_dib=sum(BAD_dib)) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,aggre_lev)

DIB_tabwo <- lgr_est_outDF2 %>%  
  filter(!PARTIAL_BIN) %>%
  group_by(dat_grp,esutype,RT_comb,reartype,defin_det_yr,grp_code,aggre_lev,bin_unit,PARTIAL_BIN,partial_prop) %>%
  summarize(
    tot_dib=sum(days_in_bin),
    tot_pot_dib=sum(pot_bin_hours/24),
    GOOD_dib=sum(days_in_bin[GOOD]),
    NONESTIM_dib=sum(days_in_bin[no_estimate]),
    INADMISS_dib=sum(days_in_bin[inadmiss_either]),
    BAD_dib=sum(days_in_bin[BAD]))  %>%
  group_by(dat_grp,esutype,RT_comb,reartype,defin_det_yr,grp_code,aggre_lev) %>%
  summarize(
    tot_dib=sum(tot_dib),
    tot_pot_dib=sum(tot_pot_dib),
    GOOD_dib=sum(GOOD_dib),
    NONESTIM_dib=sum(NONESTIM_dib),
    INADMISS_dib=sum(INADMISS_dib),
    BAD_dib=sum(BAD_dib)) %>%
  arrange(dat_grp,esutype,reartype,defin_det_yr,aggre_lev)


DIB_tab_l_comb <- rbind(
  data.frame(grp="DIB_tab_wo_l",melt(DIB_tabwo %>% select(-tot_dib,-tot_pot_dib),variable.name = "Category")),
  data.frame(grp="DIB_tab_l",melt(DIB_tabw %>% select(-tot_dib,-tot_pot_dib),variable.name = "Category")))

DIB_tab_l_comb
DIB_tab_l_comb$aggre_lev=factor(DIB_tab_l_comb$aggre_lev,levels=c("day","halfweek","week","2weeks","month","year"))


table(DIB_tab_l_comb$Category,DIB_tab_l_comb$grp)


DIB_tab_l_comb4plt <- DIB_tab_l_comb %>% filter(Category %in% c("GOOD_dib","NONESTIM_dib","INADMISS_dib") & 
                                                  aggre_lev %in% c("day","week","month") & 
                                                  grp=="DIB_tab_l" & dat_grp=="lgr_det" & defin_det_yr %in% 1998:2025)

DIB_tab_l_comb4plt$Category <- factor(DIB_tab_l_comb4plt$Category,levels=c("NONESTIM_dib","INADMISS_dib","GOOD_dib"))


DIB_tab_l_comb4plt$Category <- factor(DIB_tab_l_comb4plt$Category,levels=c("NONESTIM_dib","INADMISS_dib","GOOD_dib"))
# # filter(grp=="DIB_tab_l")
# 
# gp_lgr_BON_DIB <- ggplot() +  
#   geom_bar(data=DIB_tab_l_comb4plt,
#            aes(x=as.numeric(defin_det_yr),y=value,fill=Category,color=Category),
#                                        stat="identity",position="stack",color="black") +
#   scale_x_continuous(breaks=seq(1998,2023,3)) +
#   scale_fill_manual(values=c("darkred","orange3","green3")) +
#   facet_grid(aggre_lev~dat_grp+reartype) +#ggtitle("DIB")+
#   mytheme + 
#   geom_vline(xintercept = 2016.5) +
#   geom_vline(xintercept = 2019.5) +
#   # theme(legend.position = "bottom") + 
#   labs(y="Days",x="Year") #+ 


DIB_tab_l_comb4plt2 <- DIB_tab_l_comb4plt# %>% filter(grp=="SR_Ch1" )
DIB_tab_l_comb4plt2$RearType=ifelse(DIB_tab_l_comb4plt2$reartype=="W","Wild","Hatchery")

category_replace_tab=data.frame(old=c("NONESTIM_dib","INADMISS_dib","GOOD_dib"),
                                new=c("Too Sparse","Inadmissible","Estimable"))

# 
head(DIB_tab_l_comb4plt2$Category)
DIB_tab_l_comb4plt2$Category=category_replace_tab$new[match(as.character(DIB_tab_l_comb4plt2$Category),category_replace_tab$old)]
DIB_tab_l_comb4plt2$Category <- factor(DIB_tab_l_comb4plt2$Category,levels = (category_replace_tab$new))
DIB_tab_l_comb4plt2$Aggre_lev <- factor(stringr::str_to_title(DIB_tab_l_comb4plt2$aggre_lev),levels = c("Day","Week","Month"))
# DIB_tab_l_comb4plt2$grp <- factor(DIB_tab_l_comb4plt2$grp,levels = c("lgr_pooled","lgr_det"))

# table(DIB_tab_l_comb4plt2$reartype,DIB_tab_l_comb4plt2$dat_grp)

# 
gp_lgr_BON_DIB_rev <- ggplot(data=DIB_tab_l_comb4plt2,aes(x=as.numeric(defin_det_yr),
                                                          y=value,fill=Category,color=Category)) +  
  geom_bar(
    stat="identity",position="stack",color="black") +
  scale_x_continuous(breaks=seq(2010,2023,1)) +
  scale_fill_manual(values=alpha(c("darkred","orange3","green3"),0.5))+
  facet_grid(Aggre_lev~RearType) +  mytheme + 
  geom_vline(xintercept = 2019.5,linetype="dotted",linewidth=0.85,color="blue",alpha=0.85) +
  labs(y="Survival Monitoring Days",x="Year") + 
  theme(plot.title = element_text(face="bold",size=25,margin=margin(0,0,1,0,unit = "cm")),
        axis.text.x = element_text(angle=45,vjust = 0.5,size=14),axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),strip.text.x = element_text(size=20),legend.background = element_rect(color="black"))#,plot.margin = margin(2,1,1,1,unit = "")) #+
# geom_hline(yintercept = 28*3)

gp_lgr_BON_DIB_rev





gp_lgr_BON_DIB_rev_CH1 <- gp_lgr_BON_DIB_rev_STHD <- gp_lgr_BON_DIB_rev_SOCK <- gp_lgr_BON_DIB_rev
gp_lgr_BON_DIB_rev_CH1$data <- gp_lgr_BON_DIB_rev_CH1$data %>% filter(esutype=='SR_Ch1')
gp_lgr_BON_DIB_rev_STHD$data <- gp_lgr_BON_DIB_rev_STHD$data %>% filter(esutype=='SR_Sthd')
gp_lgr_BON_DIB_rev_SOCK$data <- gp_lgr_BON_DIB_rev_SOCK$data %>% filter(esutype=='SR_Sock')



library(gridExtra)
# png("temp/plts/LGR-MCN_CH1.png",width = 15,height=7.75,units = "in",res = 600)
gp_lgr_BON_DIB_rev_CH1 + ggtitle("Snake River Spring/Summer Chinook Salmon Juvenile Survival (LGR-MCN) ")
# dev.off()

# png("temp/plts/LGR-MCN_STHD.png",width = 15,height=7.75,units = "in",res = 600)
gp_lgr_BON_DIB_rev_STHD+ ggtitle("Snake River Steelhead Juvenile Survival (LGR-MCN) ")
# dev.off()

# png("temp/plts/LGR-MCN_SOCK.png",width = 15,height=7.5,units = "in",res = 600)
gp_lgr_BON_DIB_rev_SOCK+ ggtitle("Snake River Sockeye Salmon Juvenile Survival (LGR-MCN) ")
# dev.off()











# values = alpha(c("blue", "red"), .3)
# 
# gp_lgr_BON_n_datasets
# 
# DIB_tab_l_comb4plt2 <- DIB_tab_l_comb4plt %>% 
#   mutate(paste(esutype,reartype))
# 
# 
# 
# 
# 
# 
# ########################### #
# # Binned estimates
# ########################### #
# 
# DIB_tab_w_comb <- rbind(
#   data.frame(grp="DIB_tab_wo_w",DIB_tabwo),
#   data.frame(grp="DIB_tab_w",DIB_tabw))
# 
# 
# prop_DIB_tab_w_comb2 <- DIB_tab_w_comb %>%  
#   filter(grp=="DIB_tab_w" & aggre_lev %in% c("day","week","month")) %>%
#   mutate(era=factor(ifelse(defin_det_yr<2020,"before","after"),c("before","after"))) %>%
#   mutate(prop_days_w_ests=GOOD_dib/(GOOD_dib+BAD_dib)) %>%
#   
#   group_by(grp,esutype,RT_comb,reartype,aggre_lev,era) %>% 
#   summarize(avg_prop_days_w_ests=mean(prop_days_w_ests),
#             q25_prop_days_w_ests=quantile(prop_days_w_ests,0.25),
#             q75_prop_days_w_ests=quantile(prop_days_w_ests,0.75),
#             min_prop_days_w_ests=min(prop_days_w_ests),
#             max_prop_days_w_ests=max(prop_days_w_ests)
#             )
#   
# 
# 
# gp_lgr_BON_prop_DIB <- ggplot() +  
#   # geom_errorbar(data=prop_DIB_tab_w_comb2,
#   #            aes(x=era,ymin=min_prop_days_w_ests,ymax=max_prop_days_w_ests),
#   #            color="black",width=0) +
#   geom_errorbar(data=prop_DIB_tab_w_comb2,
#                 aes(x=era,ymin=q25_prop_days_w_ests,
#                     ymax=q75_prop_days_w_ests),
#                 color="black",width=0.25) +
#   geom_point(data=prop_DIB_tab_w_comb2,
#              aes(x=era,y=avg_prop_days_w_ests,fill=era),pch=21,color="black") +
#     facet_grid(aggre_lev~esutype+reartype) +#ggtitle("DIB")+
#   mytheme +
#   scale_y_continuous(limits=c(0,1)) +
#   labs(y="Days",x="Year") #+ 
# 
# # gp_lgr_BON_prop_DIB