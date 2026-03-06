library(ggplot2)
library(dplyr)
library(gridExtra)

source("C:/repos/repo_simCJS/simCJS/R/per2_surph_ests.R")
source("C:/repos/repo_simCJS/simCJS/R/get_est_tab.R")

# CH1_res_exrt_ls <- list()

# exclusive look at SR_CH1 det
lgr_bin_brkDF <- readRDS("comp_files/lgr_bin_brkDF")
lgr_dh_tab_year <- lgr_bin_brkDF %>% filter(dat_grp=="lgr_det" & esutype=="SR_Ch1" & binsize=="year")


CH1_lgr_bin_brkDF <- lgr_bin_brkDF %>% filter(dat_grp=="lgr_det" & esutype=="SR_Ch1") %>%  
  mutate(N=ifelse(is.na(n.11),0,n.11)+
           ifelse(is.na(n.10),0,n.10)+
           ifelse(is.na(n.01),0,n.01)+
           ifelse(is.na(n.00),0,n.00)) %>% 
  arrange(reartype,defin_det_yr)

COMB_CH1_lgr_bin_brkDF <- CH1_lgr_bin_brkDF %>% 
  group_by(dat_grp,esutype,defin_det_yr,binsize,month,weeks2,week1,days3.5,day) %>% 
  summarize(n.00=sum(n.00,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.11=sum(n.11,na.rm=T),
            n.11greq10=n.11>=10) %>%
  mutate(N=ifelse(is.na(n.11),0,n.11)+
           ifelse(is.na(n.10),0,n.10)+
           ifelse(is.na(n.01),0,n.01)+
           ifelse(is.na(n.00),0,n.00))




##################### #
# TABLE 25
##################### #

# comparison with widener 2022 report
Ch1_W_comb_tb25 <- data.frame(
  esutype="SR_Sock",
  reartype="COMB",
  year=1998:2021,
  LGR_MCN=c(0.771,0.791,0.775,0.542,0.768,0.729,0.667,0.661,0.754,0.773,0.786,0.765,0.744,0.743,0.798,0.778,0.722,0.647,0.703,0.709,0.760,0.669,0.674,0.673),
  LGR_MCN_se=c(0.015,0.014,0.014,0.028,0.026,0.02,0.023,0.017,0.01,0.013,0.020,0.018,0.021,0.015,0.020,0.018,0.015,0.058,0.017,0.020,0.031,0.028,0.073,0.053),
  MCN_BON=c(NA,0.62,0.575,0.437,0.469,0.757,0.566,0.681,0.827,0.780,0.607,0.606,0.612,0.955,0.831,0.685,0.577,0.843,0.490,0.436,0.762,0.813,0.463,0.533),
  MCN_BON_se=c(NA,0.099,0.156,0.041,0.120,0.059,0.164,0.243,0.085,0.088,0.127,0.068,0.063,0.197,0.065,0.092,0.074,0.106,0.095,0.063,0.144,0.114,0.145,0.117)
  )

Ch1_W_comb_tb25 <- Ch1_W_comb_tb25 %>% mutate(S1=LGR_MCN,defin_det_yr=year) %>% filter(year>=1998)

##################### #


### YEAR sep RT ### #
# sepRT_CH1_yr_plt <- ggplot(data= CH1_lgr_bin_brkDF %>% filter(binsize=="year"),
#                             aes(y=n.11,x=defin_det_yr,fill=n.11greq10)) +
#   geom_bar(stat="identity",color="black") + facet_grid(esutype~reartype,scales="free_y")+
#   geom_hline(yintercept = 10) +
#   ggtitle("sepRT CH1 ")

# all estimates work
CH1_lgr_bin_brk_summ_tb <- CH1_lgr_bin_brkDF %>%
  filter(binsize=="year") %>%   
  mutate(N=n.11+n.10+n.01+n.00) %>%
  group_by(dat_grp,esutype,reartype) %>%
  summarize(
    n_admiss_yrs=sum(n.11greq10),
    tot_yrs=length(n.11greq10),
    prop_admiss=sum(n.11greq10)/length(n.11greq10))


# CH1_RT_COMB_lgr_dh_tab_year <- lgr_dh_tab_year %>% 
#   filter(dat_grp=="lgr_det" & esutype=="SR_CH1") %>% 
#   group_by(dat_grp,esutype,defin_det_yr) %>%
#   summarize(n.00=sum(n.00,na.rm=T),
#             n.10=sum(n.10,na.rm=T),
#             n.01=sum(n.01,na.rm=T),
#             n.11=sum(n.11,na.rm=T)) %>%
#   mutate(n.11greq10=n.11>=10)
# 
# 
# CH1_RT_COMB_lgr_bin_brk_summ_tb <- CH1_RT_COMB_lgr_dh_tab_year %>%
#   mutate(N=n.11+n.10+n.01+n.00) %>%
#   group_by(dat_grp,esutype) %>%
#   summarize(
#     n_admiss_yrs=sum(n.11greq10),
#     tot_yrs=length(n.11greq10),
#     prop_admiss=sum(n.11greq10)/length(n.11greq10))
# 
# COMB_CH1_yrs_wo_ests=CH1_RT_COMB_lgr_dh_tab_year %>% filter(!n.11greq10) %>% pull(defin_det_yr)
# 
# 
# CH1_res_exrt_ls <- c(CH1_res_exrt_ls,paste("The n>=10 threshold was exceeded by ",CH1_lgr_bin_brk_summ_tb$reartype[1],"CH1eye salmon in ",CH1_lgr_bin_brk_summ_tb$n_admiss_yrs[1],"study years"))
# CH1_res_exrt_ls <- c(CH1_res_exrt_ls,paste("The n>=10 threshold was exceeded by ",CH1_lgr_bin_brk_summ_tb$reartype[2],"CH1eye salmon in ",CH1_lgr_bin_brk_summ_tb$n_admiss_yrs[2],"study years"))



combRT_CH1_yr_plt <- ggplot(data=CH1_lgr_bin_brkDF,
                             aes(y=n.11,x=defin_det_yr,fill=n.11greq10)) +
  geom_bar(stat="identity",color="black") + 
  facet_grid(~esutype,scales="free_y")+
  geom_hline(yintercept = 10) + 
  ggtitle("comb RT CH1 ")


# CH1_res_exrt_ls[["sepRT_CH1_yr_plt"]] <- sepRT_CH1_yr_plt
# CH1_res_exrt_ls[["combRT_CH1_yr_plt"]] <- combRT_CH1_yr_plt



############################################### #
# minimum scale for separate rear types
############################################### #

CH1_lgr_bin_brkDF_admiss_summ_tb1 <- CH1_lgr_bin_brkDF %>% 
  filter(binsize!="year") %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,binsize) %>%#,month,weeks2,week1,days3.5,day) %>%
  summarize(admiss_bins=sum(n.11greq10),
            nbins=length(n.11greq10),
            Nsum=sum(N,na.rm=T),
            tg_contrib=sum(N[n.11greq10],na.rm=T),
            prop_bin_pcontrib=admiss_bins/nbins
  ) %>%
  left_join( CH1_lgr_bin_brkDF %>% 
               filter(binsize=="year") %>% select(dat_grp,esutype,reartype,defin_det_yr,N)) %>%
  mutate(
    pcontrib=tg_contrib/N,
    pcontribgreq0.8=pcontrib>=0.8,
    tree_depth=match(binsize,c("month","weeks2","week1","days3.5","day")))

CH1_lgr_contrib_binsDF <- CH1_lgr_bin_brkDF_admiss_summ_tb1 %>% 
  filter(pcontribgreq0.8 & admiss_bins>1) %>% # more than one admissible bin
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>%
  summarize(
    tree_depth_max=max(tree_depth),
    # identifys sub-estimates to interpret
    min_temp_scale=c("month","weeks2","week1","days3.5","day")[tree_depth_max],
    pcontrib_lst=pcontrib[which.max(tree_depth)],
    tg_contrib_lst=tg_contrib[which.max(tree_depth)],
    tot_tgs=Nsum[which.max(tree_depth)],
    annual_n=N[which.max(tree_depth)])


CH1_lgr_contrib_binsDF <- CH1_lgr_contrib_binsDF %>% 
  mutate(code=paste(dat_grp,esutype,reartype,defin_det_yr,min_temp_scale))

# CH1_lgr_bin_brkDF_admiss_summ_tb1 <- CH1_lgr_bin_brkDF_admiss_summ_tb1 #%>% 
  # mutate(code=paste(dat_grp,esutype,defin_det_yr,binsize))

CH1_lgr_bin_brkDF <- CH1_lgr_bin_brkDF %>%
  mutate(code=paste(dat_grp,esutype,reartype,defin_det_yr,binsize))

# filtering down to codes with contributions

CH1_all_minDH <- CH1_lgr_bin_brkDF %>% 
  filter(code %in% CH1_lgr_contrib_binsDF$code) %>% 
  left_join(CH1_lgr_contrib_binsDF)

length(unique(CH1_lgr_contrib_binsDF$code))

# messed up
# CH1_all_minDH <- data.frame(CH1_all_minDH,
#                             CH1_lgr_contrib_binsDF[match(CH1_all_minDH$code,CH1_lgr_contrib_binsDF$code),
#                                                        c("min_temp_scale","pcontrib_lst","tg_contrib_lst","tot_tgs","annual_n")])
# 
# head(data.frame(CH1_all_minDH))
# CH1_all_minDH$min_temp_scale

CH1_estim_minDH <- CH1_all_minDH %>% filter(n.11greq10)


CH1_estim_minDH$bin_id <- apply(CH1_estim_minDH[,c("month","weeks2","week1","days3.5","day")],
                              1,function(x) x[which(!is.na(x))])

CH1_estim_minDH <- CH1_estim_minDH %>% 
  relocate(dat_grp,esutype,defin_det_yr,bin_id) %>% 
  select(-month,-weeks2,-week1,-days3.5)


# CH1_estim_minDH$bin_id <- apply(CH1_estim_minDH[,c("month","weeks2","week1","days3.5","day")],
#                                 1,function(x) x[which(!is.na(x))])
# CH1_estim_minDH$min_temp_scale
# head(CH1_estim_minDH)
# CH1_estim_minDH$min_temp_scale <- CH1_estim_minDH$binsize

# CH1_lgr_bin_brkDF %>% filter(binsize=="year" & n.11greq10)  %>% mutate(min_temp_scale="year")


################################################################################# #
# processed detection data sets (i.e., post n11 and P_contrib algorithm)
################################################################################# #


CH1_years_onlyDF <- CH1_lgr_bin_brkDF %>%
  filter(binsize=="year" & n.11greq10)  %>% 
  mutate(min_temp_scale="year")

# CH1_testCOMB <- bind_rows(CH1_estim_minDH,CH1_years_onlyDF)


################################################################################# #
# computing MLEs for all of the filtered models
################################################################################# #

# Important
CH1_estim_minDH[is.na(CH1_estim_minDH$n.01),"n.01"]=0

CH1_estim_minDH_estsDF  <- data.frame(CH1_estim_minDH,get_est_tab(df_in = CH1_estim_minDH)) %>% 
  mutate(wt1=N/tg_contrib_lst,
         S1CV=s1SE/S1,
         wt2_unadj=1/(S1CV^2))

# looking into an issue
# dud_ests2009 <- get_est_tab(df_in = CH1_estim_minDH %>% filter(is.na(n.01)),only_cell_count_ls = T)[[1]]
# dud_ests2009["n.01"]=0
# per2_surph_ests(dud_ests2009)





# pooled estimate
CH1_years_only_pool_estsDF <- data.frame(CH1_years_onlyDF,get_est_tab(df_in = CH1_years_onlyDF))

# weighted average annual estimate
# CH1_years_only_pool_estsDF <- data.frame(CH1_years_onlyDF,get_est_tab(df_in = CH1_years_onlyDF))
head(CH1_estim_minDH_estsDF)

CH1_wtavgDF1 <- CH1_estim_minDH_estsDF %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>% 
  summarize(S1_wtavg1=sum(wt1*S1),
            S1_wtSE1=sqrt(sum((wt1^2)*(s1SE^2))),
            binsize=unique(min_temp_scale))

# Burnham et al. 1987 pg. 259
wt2_sum_tb <- CH1_estim_minDH_estsDF %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>%
  summarize(wt2_sum=sum(wt2_unadj))

CH1_wtavgDF2 <- CH1_estim_minDH_estsDF %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>% 
  left_join(wt2_sum_tb %>% 
              select(dat_grp,esutype,reartype,defin_det_yr,wt2_sum)) %>%
  mutate(wt2=wt2_unadj/wt2_sum) %>%
  summarize(S1_wtavg2=sum(wt2*S1),
            S1_wtSE2=sqrt(sum((wt2^2)*(s1SE^2))),
            binsize=unique(min_temp_scale))

CH1_wtavg_comb_w <- CH1_wtavgDF1 %>% 
  left_join(CH1_years_only_pool_estsDF %>% 
              select(dat_grp,esutype,reartype,defin_det_yr,S1,s1SE)) %>%
  left_join(CH1_wtavgDF2)



CH1_wtAVG_tb_l <-CH1_years_only_pool_estsDF %>% 
  mutate(est_ver="pooled") %>%
  select(dat_grp,esutype,reartype,defin_det_yr,est_ver,binsize,S1,s1SE) %>%
  bind_rows(CH1_wtavgDF1 %>% 
              rename(S1=S1_wtavg1,s1SE=S1_wtSE1) %>%
              mutate(est_ver="wt1_ntgs") %>%
              select(dat_grp,esutype,reartype,defin_det_yr,binsize,est_ver,S1,s1SE)) %>%
  bind_rows(CH1_wtavgDF2 %>% 
              rename(S1=S1_wtavg2,s1SE=S1_wtSE2) %>%
              mutate(est_ver="wt2_CV") %>%
              select(dat_grp,esutype,reartype,defin_det_yr,binsize,est_ver,S1,s1SE)) %>%
  mutate(min_temp_scale=binsize)
  

ggplot() +
  #color=factor(bin_id),
  geom_errorbar(data=CH1_wtAVG_tb_l,
                aes(x=factor(defin_det_yr),
                    group=est_ver,
                    y=S1,
                    ymin=S1-s1SE*1.96,
                    ymax=S1+s1SE*1.96),
                width=0,
                position = position_dodge(width=0.5),
                linewidth=0.5) +
  geom_point(data=CH1_wtAVG_tb_l,
             aes(x=factor(defin_det_yr),
                 fill=est_ver,
                 size=min_temp_scale,
                 y=S1),
             position = position_dodge(width=0.5),
             shape=21,size=2.5) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted") +
  facet_wrap(~reartype,ncol=1)



CH1_wtAVG_tb_l_wt2 <- CH1_wtAVG_tb_l %>% 
  filter(est_ver=="wt2_CV"| (est_ver=="pooled" & defin_det_yr %in% c(2022))) %>%
  mutate(binsize=factor(binsize,levels=c("day","days3.5","week1","weeks2","month","year")))


ggplot() +
  geom_errorbar(data=CH1_wtAVG_tb_l_wt2,
                aes(x=factor(defin_det_yr),
                    y=S1,
                    ymin=S1-s1SE*1.96,
                    ymax=S1+s1SE*1.96),
                width=0,
                position = position_dodge(width=0.5),
                color="black",linewidth=0.5) +
  geom_point(data=CH1_wtAVG_tb_l_wt2,
             aes(x=factor(defin_det_yr),
                 fill=binsize,
                 size=binsize,
                 y=S1),
             position = position_dodge(width=0.5),
             shape=21) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted") +
  facet_wrap(~reartype,ncol=1)+
  scale_fill_manual(values=c("red3","red2","red","tomato3","tomato","orange"))+
  scale_size_manual(values=c(2,3,4,5,6,7))
# scale_fill_manual(values=c("blue4","blue3","blue","cadetblue4","cadetblue3","cyan")) +
#   scale_size_manual(values=c(2,3,4,5,6,7))




# pale
# library(RColorBrewer)






# CH1_testCOMB_estsDF4plt <- testCOMB_estsDF %>% 
#   filter(min_temp_scale!="year") #%>%

(CH1_comb_S1 <- ggplot() +
  #color=factor(bin_id),
    geom_errorbar(data=CH1_wtAVG_tb_l,
               aes(x=factor(defin_det_yr),
                   # size=min_temp_scale,
                   # group=bin_id,
                   y=S1,
                   ymin=S1-s1SE*1.96,
                   ymax=S1+s1SE*1.96),
               # position = position_dodge(width=0.5),
               color="green4",linewidth=1) +
    geom_point(data=CH1_years_only_pool_estsDF,
               aes(x=factor(defin_det_yr),
                   # size=min_temp_scale,
                   y=S1),
               # position = position_dodge(width=0.5),
               shape=21,fill="green4",size=4) +
  geom_errorbar(data=CH1_wtAVG_tb_l,
                aes(x=factor(defin_det_yr),
                    fill=min_temp_scale ,
                    shape=min_temp_scale ,
                    group=bin_id,
                    y=S1,
                    ymin=S1-s1SE*1.96,
                    ymax=S1+s1SE*1.96),
                width=0,position = position_dodge(width=0.5),size=0.05) +
  geom_point(data=CH1_estim_minDH_estsDF,
             aes(x=factor(defin_det_yr),
                 fill=min_temp_scale ,
                 shape=min_temp_scale ,
                 size=min_temp_scale,
                 group=bin_id,
                 y=S1,
                 ymin=S1-s1SE*1.96,
                 ymax=S1+s1SE*1.96),
             position = position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,max(CH1_estim_minDH_estsDF$S1+CH1_estim_minDH_estsDF$s1SE*2.0))) + 
  # scale_color_manual(values=c(rep("gray30",3),"black")) + 
  # scale_size_manual(values = c(3,rep(1.5,5))) +
  scale_size_manual(values = c(2,2,2,2,2,2)) +
  scale_shape_manual(values=c(21,22,23,24,25,25)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")+
  facet_wrap(~reartype,ncol=1))


# testCOMB_estsDF$p1SE
CH1_comb_P2 <- ggplot(data=testCOMB_estsDF %>% 
                        mutate(min_temp_scale=factor(min_temp_scale,levels=rev(c("day","days3.5","week1","weeks2","month","year")))),
                      aes(x=factor(defin_det_yr),
                          fill=min_temp_scale ,
                          shape=min_temp_scale ,
                          size=min_temp_scale,
                          group=bin_id,
                          y=p1,
                          ymin=p1-s1SE*1.96,
                          ymax=p1+s1SE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5),size=0.05) + 
  geom_point(position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(testCOMB_estsDF$S1+testCOMB_estsDF$s1SE*2.0))) + 
  # scale_color_manual(values=c(rep("gray30",3),"black")) + 
  scale_size_manual(values = c(3,rep(1.5,5))) +
  scale_shape_manual(values=c(21,22,23,24,25,25)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")+
  facet_wrap(~reartype,ncol=1)


CH1_comb_LAMBDA <- ggplot(data=testCOMB_estsDF %>% 
                            mutate(min_temp_scale=factor(min_temp_scale,levels=rev(c("day","days3.5","week1","weeks2","month","year")))),
                          aes(x=factor(defin_det_yr),
                              fill=min_temp_scale ,
                              shape=min_temp_scale ,
                              size=min_temp_scale,
                              group=bin_id,
                              y=lambda,
                              ymin=lambda-lambdaSE*1.96,
                              ymax=lambda+lambdaSE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5),size=0.05) + 
  geom_point(position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(testCOMB_estsDF$S1+testCOMB_estsDF$s1SE*2.0))) + 
  # scale_color_manual(values=c(rep("gray30",3),"black")) + 
  scale_size_manual(values = c(3,rep(1.5,5))) +
  scale_shape_manual(values=c(21,22,23,24,25,25)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")+
  facet_wrap(~reartype,ncol=1)

grid.arrange(
  CH1_comb_S1 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  CH1_comb_P2 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  CH1_comb_LAMBDA + geom_hline(yintercept = c(0,0.5,1),linetype="dotted")
)

CH1_comb_S1 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted")
 
