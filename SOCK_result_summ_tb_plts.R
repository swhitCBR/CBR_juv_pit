library(ggplot2)
library(dplyr)
library(gridExtra)

source("C:/repos/repo_simCJS/simCJS/R/per2_surph_ests.R")
source("C:/repos/repo_simCJS/simCJS/R/get_est_tab.R")

sock_res_exrt_ls <- list()

lgr_bin_brkDF <- readRDS("comp_files/lgr_bin_brkDF")
lgr_dh_tab_year <- lgr_bin_brkDF %>% filter(dat_grp=="lgr_det" & esutype=="SR_Sock" & binsize=="year")
SOCK_lgr_bin_brkDF <- lgr_bin_brkDF %>% filter(dat_grp=="lgr_det" & esutype=="SR_Sock") %>%  mutate(N=n.11+n.10+n.01+n.00)  %>% arrange(reartype,defin_det_yr)


COMB_SOCK_lgr_bin_brkDF <- SOCK_lgr_bin_brkDF %>% 
  group_by(dat_grp,esutype,defin_det_yr,binsize,month,weeks2,week1,days3.5,day) %>% 
  summarize(n.00=sum(n.00,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.11=sum(n.11,na.rm=T),
            n.11greq10=n.11>=10) %>%
  mutate(N=n.11+n.10+n.01+n.00)

##################### #
# TABLE 29
##################### #

# comparison with widener 2022 report
sock_comb_tb29 <- data.frame(
  esutype="SR_Sock",
  reartype="COMB",
  year=1996:2021,
  LGR_MCN=c(0.283,NA,0.689,0.655,0.679,0.205,0.524,0.669,0.741,0.388,0.630,0.679,0.763,0.749,0.723,0.659,0.762,0.691,0.873,0.702,0.523,0.544,0.684,0.836,0.803,0.817),
  LGR_MCN_se=c(0.184,NA,0.157,0.083,0.110,0.063,0.062,0.054,0.254,0.078,0.083,0.066,0.103,0.032,0.039,0.033,0.032,0.043,0.054,0.054,0.047,0.081,0.061,0.053,0.111,0.094),
  MCN_BON=c(NA,NA,0.142,0.841,0.206,0.105,0.684,0.551,NA,NA,1.113,0.259,0.544,0.765,0.752,NA,0.619,0.776,0.817,0.531,0.227,0.324,0.940,0.520,0.546,0.452),
  MCN_BON_se=c(NA,NA,0.099,0.584,0.110,0.050,0.432,0.144,NA,NA,0.652,0.084,0.262,0.101,0.098,NA,0.084,0.106,0.115,0.115,0.059,0.107,0.151,0.044,0.149,0.067))

sock_comb_tb29 <- sock_comb_tb29 %>% mutate(S1=LGR_MCN,defin_det_yr=year) %>% filter(year>=1998)

##################### #

### YEAR sep RT s### #
sepRT_Sock_yr_plt <- ggplot(data= SOCK_lgr_bin_brkDF %>% filter(binsize=="year"),
       aes(y=n.11,x=defin_det_yr,fill=n.11greq10)) +
  geom_bar(stat="identity",color="black") + facet_grid(esutype~reartype,scales="free_y")+
  geom_hline(yintercept = 10) +
  ggtitle("sepRT Sockeye ")


SOCK_lgr_bin_brk_summ_tb <- SOCK_lgr_bin_brkDF %>%
  filter(binsize=="year") %>%   
  mutate(N=n.11+n.10+n.01+n.00) %>%
  group_by(dat_grp,esutype,reartype) %>%
  summarize(
    n_admiss_yrs=sum(n.11greq10),
    tot_yrs=length(n.11greq10),
    prop_admiss=sum(n.11greq10)/length(n.11greq10))


SOCK_RT_COMB_lgr_dh_tab_year <- lgr_dh_tab_year %>% 
  filter(dat_grp=="lgr_det" & esutype=="SR_Sock") %>% 
  group_by(dat_grp,esutype,defin_det_yr) %>%
  summarize(n.00=sum(n.00,na.rm=T),
            n.10=sum(n.10,na.rm=T),
            n.01=sum(n.01,na.rm=T),
            n.11=sum(n.11,na.rm=T)) %>%
  mutate(n.11greq10=n.11>=10)



# sock_res_exrt_ls[["SOCK_RT_COMB_lgr_dh_tab_year"]] <- SOCK_RT_COMB_lgr_dh_tab_year

SOCK_RT_COMB_lgr_bin_brk_summ_tb <- SOCK_RT_COMB_lgr_dh_tab_year %>%
  mutate(N=n.11+n.10+n.01+n.00) %>%
  group_by(dat_grp,esutype) %>%
  summarize(
    n_admiss_yrs=sum(n.11greq10),
    tot_yrs=length(n.11greq10),
    prop_admiss=sum(n.11greq10)/length(n.11greq10))

COMB_sock_yrs_wo_ests=SOCK_RT_COMB_lgr_dh_tab_year %>% filter(!n.11greq10) %>% pull(defin_det_yr)


sock_res_exrt_ls <- c(sock_res_exrt_ls,paste("The n>=10 threshold was exceeded by ",SOCK_lgr_bin_brk_summ_tb$reartype[1],"Sockeye salmon in ",SOCK_lgr_bin_brk_summ_tb$n_admiss_yrs[1],"study years"))
sock_res_exrt_ls <- c(sock_res_exrt_ls,paste("The n>=10 threshold was exceeded by ",SOCK_lgr_bin_brk_summ_tb$reartype[2],"Sockeye salmon in ",SOCK_lgr_bin_brk_summ_tb$n_admiss_yrs[2],"study years"))


# sock_res_exrt_ls[[2]] <- paste("The n>=10 threshold was exceeded by ",SOCK_lgr_bin_brk_summ_tb$reartype[2],"Sockeye salmon in ",SOCK_lgr_bin_brk_summ_tb$n_admiss_yrs[2],"study years")
# sock_res_exrt_ls[[1]] <- paste("The n>=10 threshold was exceeded by ",SOCK_lgr_bin_brk_summ_tb$reartype[1],"Sockeye salmon in ",SOCK_lgr_bin_brk_summ_tb$n_admiss_yrs[1],"study years")
# sock_res_exrt_ls[[2]] <- paste("The n>=10 threshold was exceeded by ",SOCK_lgr_bin_brk_summ_tb$reartype[2],"Sockeye salmon in ",SOCK_lgr_bin_brk_summ_tb$n_admiss_yrs[2],"study years")
# sock_res_exrt_ls[[3]] <- paste("When hatchery and wild reartypes of Sockeye salmon were pooled the n>=10 threshold met across a total of ",SOCK_RT_COMB_lgr_bin_brk_summ_tb$n_admiss_yrs[1],"study years")
# sock_res_exrt_ls[[4]] <- paste("The",length(COMB_sock_yrs_wo_ests),"years where Sockeye Salmon survial estimates were not generatd were:",paste(COMB_sock_yrs_wo_ests,collapse = ","))


combRT_Sock_yr_plt <- ggplot(data=SOCK_RT_COMB_lgr_dh_tab_year,
       aes(y=n.11,x=defin_det_yr,fill=n.11greq10)) +
  geom_bar(stat="identity",color="black") + 
  facet_grid(~esutype,scales="free_y")+
  geom_hline(yintercept = 10) + 
  ggtitle("comb RT Sockeye ")


sock_res_exrt_ls[["sepRT_Sock_yr_plt"]] <- sepRT_Sock_yr_plt
sock_res_exrt_ls[["combRT_Sock_yr_plt"]] <- combRT_Sock_yr_plt



############################################### #
# minimum scale for separate rear types
############################################### #

SOCK_lgr_bin_brkDF_admiss_summ_tb1 <- SOCK_lgr_bin_brkDF %>% 
  filter(binsize!="year") %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr,binsize) %>%#,month,weeks2,week1,days3.5,day) %>%
  summarize(admiss_bins=sum(n.11greq10),
            nbins=length(n.11greq10),
            Nsum=sum(N,na.rm=T),
            tg_contrib=sum(N[n.11greq10],na.rm=T),
            prop_bin_pcontrib=admiss_bins/nbins
            ) %>%
  left_join( SOCK_lgr_bin_brkDF %>% 
               filter(binsize=="year") %>% select(dat_grp,esutype,reartype,defin_det_yr,N)) %>%
  mutate(
    pcontrib=tg_contrib/N,
    pcontribgreq0.8=pcontrib>=0.8,
    tree_depth=match(binsize,c("month","weeks2","week1","days3.5","day")))

SOCK_lgr_contrib_binsDF <- SOCK_lgr_bin_brkDF_admiss_summ_tb1 %>% 
  filter(pcontribgreq0.8 & admiss_bins>1) %>% # more than one admissible bin
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>%
  summarize(
    tree_depth_max=max(tree_depth),
    min_temp_scale=c("month","weeks2","week1","days3.5","day")[tree_depth_max],
    pcontrib_lst=pcontrib[which.max(tree_depth)],
    tg_contrib_lst=tg_contrib[which.max(tree_depth)],
    tot_tgs=Nsum[which.max(tree_depth)])


SOCK_lgr_contrib_binsDF <- SOCK_lgr_contrib_binsDF %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,min_temp_scale))

SOCK_lgr_bin_brkDF_admiss_summ_tb1 <- SOCK_lgr_bin_brkDF_admiss_summ_tb1 %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,binsize))

SOCK_lgr_bin_brkDF <- SOCK_lgr_bin_brkDF %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,binsize))

SOCK_all_minDH <- SOCK_lgr_bin_brkDF %>% 
  filter(code %in% SOCK_lgr_contrib_binsDF$code) %>% 
  left_join(SOCK_lgr_contrib_binsDF)

SOCK_estim_minDH <- SOCK_all_minDH %>% filter(n.11greq10)
SOCK_estim_minDH$bin_id <- apply(SOCK_estim_minDH[,c("month","weeks2","week1","days3.5","day")],1,function(x) x[which(!is.na(x))])


testCOMB <- bind_rows(SOCK_estim_minDH,SOCK_lgr_bin_brkDF %>% filter(binsize=="year" & n.11greq10)  %>% mutate(min_temp_scale="year")) 

testCOMB_estsDF <- data.frame(testCOMB,get_est_tab(df_in = testCOMB))


sock_comb_S1 <- ggplot(data=testCOMB_estsDF,
                       aes(x=factor(defin_det_yr),
                           fill=min_temp_scale ,
                           color=min_temp_scale ,
                           shape=reartype,
                           # size=min_temp_scale,
                           group=bin_id,
                           y=S1,
                           ymin=S1-s1SE*1.96,
                           ymax=S1+s1SE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(size=3,position = position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,max(testCOMB_estsDF$S1+testCOMB_estsDF$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black")) + 
  scale_size_manual(values = c(rep(1.5,3),3)) +
  scale_shape_manual(values=c(21,22,23,24)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")


# testCOMB_estsDF$p1SE
sock_comb_P2 <- ggplot(data=testCOMB_estsDF,
                       aes(x=factor(defin_det_yr),
                           fill=min_temp_scale ,
                           color=min_temp_scale ,
                           group=bin_id,
                           shape=reartype,
                           y=p1,
                           ymin=p1-p1SE*1.96,
                           ymax=p1+p1SE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(size=3,shape=21,position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(COMB_SOCK_estimDF_w$S1+COMB_SOCK_estimDF_w$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black"))+
  scale_shape_manual(values=c(21,22,23,24)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")



sock_comb_LAMBDA <- ggplot(data=testCOMB_estsDF,
                           aes(x=factor(defin_det_yr),
                               fill=min_temp_scale ,
                               color=min_temp_scale ,
                               shape=reartype,
                               group=bin_id,
                               y=lambda,
                               ymin=lambda-lambdaSE*1.96,
                               ymax=lambda+lambdaSE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(size=3,shape=21,position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(COMB_SOCK_estimDF_w$S1+COMB_SOCK_estimDF_w$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black"))+
  scale_shape_manual(values=c(21,22,23,24)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")


grid.arrange(
  sock_comb_S1 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  sock_comb_P2 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  sock_comb_LAMBDA + geom_hline(yintercept = c(0,0.5,1),linetype="dotted")
)





















COMB_SOCK_lgr_bin_brkDF_admiss_summ_tb1 <- COMB_SOCK_lgr_bin_brkDF %>% 
  filter(binsize!="year") %>%
  group_by(dat_grp,esutype,defin_det_yr,binsize) %>%#,month,weeks2,week1,days3.5,day) %>%
  summarize(admiss_bins=sum(n.11greq10),
            nbins=length(n.11greq10),
            Nsum=sum(N,na.rm=T),
            tg_contrib=sum(N[n.11greq10],na.rm=T),
            prop_bin_pcontrib=admiss_bins/nbins) %>%
  left_join( COMB_SOCK_lgr_bin_brkDF %>% 
               filter(binsize=="year") %>%
               ungroup() %>% 
               select(dat_grp,esutype,defin_det_yr,N)) %>%
  mutate(
    pcontrib=tg_contrib/N,
    pcontribgreq0.8=pcontrib>=0.8,
    tree_depth=match(binsize,c("month","weeks2","week1","days3.5","day")))


COMB_SOCK_lgr_contrib_binsDF <- COMB_SOCK_lgr_bin_brkDF_admiss_summ_tb1 %>% 
  filter(pcontribgreq0.8 & admiss_bins>1) %>% # monre than one admissible bin
  group_by(dat_grp,esutype,defin_det_yr) %>%
  summarize(
    tree_depth_max=max(tree_depth),
    min_temp_scale=c("month","weeks2","week1","days3.5","day")[tree_depth_max],
    pcontrib_lst=pcontrib[which.max(tree_depth)],
    tg_contrib_lst=tg_contrib[which.max(tree_depth)],
    tot_tgs=Nsum[which.max(tree_depth)],
        reartype="COMB")


COMB_SOCK_lgr_contrib_binsDF <- COMB_SOCK_lgr_contrib_binsDF %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,min_temp_scale))

COMB_SOCK_lgr_bin_brkDF_admiss_summ_tb1 <- COMB_SOCK_lgr_bin_brkDF_admiss_summ_tb1 %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,binsize))

COMB_SOCK_lgr_bin_brkDF <- COMB_SOCK_lgr_bin_brkDF %>% 
  mutate(code=paste(dat_grp,esutype,defin_det_yr,binsize))

COMB_SOCK_all_minDH <- COMB_SOCK_lgr_bin_brkDF %>% 
  filter(code %in% COMB_SOCK_lgr_contrib_binsDF$code) %>% 
  left_join(COMB_SOCK_lgr_contrib_binsDF)

COMB_SOCK_estim_minDH <- COMB_SOCK_all_minDH %>% filter(n.11greq10)

COMB_SOCK_estim_minDH
COMB_SOCK_estim_minDH$bin_id <- apply(COMB_SOCK_estim_minDH[,c("month","weeks2","week1","days3.5","day")],1,function(x) x[which(!is.na(x))])




testCOMB <- bind_rows(COMB_SOCK_estim_minDH,
          COMB_SOCK_lgr_bin_brkDF %>% filter(binsize=="year" & n.11greq10)  %>% mutate(min_temp_scale="year")) 

testCOMB_estsDF <- data.frame(testCOMB,get_est_tab(df_in = testCOMB))

sock_comb_S1 <- ggplot(data=testCOMB_estsDF,
       aes(x=factor(defin_det_yr),
           fill=min_temp_scale ,
           color=min_temp_scale ,
           # size=min_temp_scale,
           group=bin_id,
           y=S1,
           ymin=S1-s1SE*1.96,
           ymax=S1+s1SE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(shape=21,size=3,position = position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,max(COMB_SOCK_estimDF_w$S1+COMB_SOCK_estimDF_w$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black")) + 
  scale_size_manual(values = c(rep(1.5,3),3)) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")


# testCOMB_estsDF$p1SE
sock_comb_P2 <- ggplot(data=testCOMB_estsDF,
       aes(x=factor(defin_det_yr),
           fill=min_temp_scale ,
           color=min_temp_scale ,
           group=bin_id,
           y=p1,
           ymin=p1-p1SE*1.96,
           ymax=p1+p1SE*1.96)) +
  #color=factor(bin_id),
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(size=3,shape=21,position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(COMB_SOCK_estimDF_w$S1+COMB_SOCK_estimDF_w$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black"))+
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")


sock_comb_LAMBDA <- ggplot(data=testCOMB_estsDF,
                       aes(x=factor(defin_det_yr),
                           fill=min_temp_scale ,
                           color=min_temp_scale ,
                           group=bin_id,
                           y=lambda,
                           ymin=lambda-lambdaSE*1.96,
                           ymax=lambda+lambdaSE*1.96)) +
  geom_errorbar(width=0,position = position_dodge(width=0.5)) + 
  geom_point(size=3,shape=21,position = position_dodge(width=0.5)) +
  # scale_y_continuous(limits=c(0,max(COMB_SOCK_estimDF_w$S1+COMB_SOCK_estimDF_w$s1SE*2.0))) + 
  scale_color_manual(values=c(rep("gray30",3),"black")) +
  geom_hline(yintercept = c(0,0.5,1),linetype="dotted")

grid.arrange(
  sock_comb_S1 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  sock_comb_P2 + geom_hline(yintercept = c(0,0.5,1),linetype="dotted"),
  sock_comb_LAMBDA + geom_hline(yintercept = c(0,0.5,1),linetype="dotted")
)





