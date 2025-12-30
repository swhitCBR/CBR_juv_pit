require(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(gridExtra)

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



MCN_BON_2perDF <- readRDS("est_files/MCN_BON_per_2_mods_outDF_sep.rds" )
MCN_BON_2perDF <- MCN_BON_2perDF[,-c(which(names(MCN_BON_2perDF)=="p1")[2],which(names(MCN_BON_2perDF)=="lambda")[2])]
MCN_BON_2perDF


bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

mcn_est_outDF3 <- readRDS("est_files/mcn_est_outDF3.rds")
lgr_est_outDF3 <- readRDS("est_files/lgr_est_outDF3.rds")

bin_tab_sub <- bin_tab_ls_combDFwYR %>% filter(bin %in% c("day","week1","weeks2","days3.5","month"))



# bin_tab_ls_combDFwYR <- readRDS("temp/data_9823_comp_SI_test/bin_tab_ls_combDFwYR_wPD568.rds")
mcn_dh_tab2 <- readRDS("comp_files/mcn_dh_tab2_9825_wPD568.rds")
mcn_dh_tab2$DH_red <- factor(paste(mcn_dh_tab2$BON,mcn_dh_tab2$Estuary),
                             levels=c("1 1","1 0","0 1","0 0"))

source("R/CJS_aggre_est_2per.R")

source("R/CJS_aggre_fxns.R")
source("R/get_CJS_count_ls.R")
source("R/get_mom_mat.R")


bt=proc.time()
mcn_det_all_out <- CJS_aggre_est_2per(dat_in=mcn_dh_tab2,DH_cols="DH_red")
proc.time()-bt

# redundant files
lapply(mcn_det_all_out,dim)

mcn_est_outDF <- mcn_det_all_out$"outDF"


# mcn_det_all_out$
mcn_det_all_out$summ_tab1
mcn_det_all_out$summ_tab2

mcn_est_outDF2 <- mcn_est_outDF %>%
  mutate(bin_code=paste(grp_code,bin,binID)) %>% 
  left_join(bin_tab_ls_combDFwYR %>% 
              mutate(bin_code=paste(grp_code,bin,binID)))

####################### #
# LGR Det and pooled
####################### #
lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
lgr_dh_tab2$DH_red <- factor(paste(lgr_dh_tab2$MCN,lgr_dh_tab2$BON),
                             levels=c("1 1","1 0","0 1","0 0"))
bt=proc.time()
lgr_det_all_out <- CJS_aggre_est_2per(dat_in=lgr_dh_tab2,DH_cols="DH_red")
proc.time()-bt

# lapply(lgr_det_all_out,dim)

lgr_est_outDF <- lgr_det_all_out$"outDF"
lgr_det_all_out$summ_tab1
lgr_det_all_out$summ_tab2

lgr_est_outDF2 <- lgr_est_outDF %>%
  mutate(bin_code=paste(grp_code,bin,binID)) %>% 
  left_join(bin_tab_ls_combDFwYR %>% 
              mutate(bin_code=paste(grp_code,bin,binID)))


# per2_surph_ests
source('R/get_ests_from_MSS.R')

# datasets with bins capture histories etc
mcn_est_outDF3 <- data.frame(mcn_est_outDF2,t(sapply(1:nrow(mcn_est_outDF2),function(x) per2_surph_ests(cell_vals_in = unlist(lgr_est_outDF2[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))
lgr_est_outDF3 <- data.frame(lgr_est_outDF2,t(sapply(1:nrow(lgr_est_outDF2),function(x) per2_surph_ests(cell_vals_in = unlist(lgr_est_outDF2[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))

comb_df <- bind_rows(mcn_est_outDF3,lgr_est_outDF3)

head(comb_df)

###################### #
# inputs to function
###################### #

admiss_thrsh=1.3
eg_tab <- expand.grid(
  dat_grp=c("mcn_det","lgr_det","lgr_pooled"),
  esutype=c("SR_Sthd","SR_Ch1","SR_Sock"),
  reartype=c("W","H"))

my_bins=c("day","days3.5","week1","weeks2","month","year")
lgr_est_outDF2$s1



table(
  is.na(mcn_est_outDF3$s1),
  is.na(mcn_est_outDF3$s1SE))
mcn_est_outDF3$s1

my_bins=c("day","days3.5","week1","weeks2","month")

ii=1
eg_tab[ii,]


# avian_recov_tagids <- readRDS("temp/avian_recov_tagids_9624.rds")
# comb_df

if(!dir.exists("pngs/temp_and_est_plts")){dir.create("pngs/temp_and_est_plts",recursive = T)}


for(ii in 1:nrow(eg_tab)){
  
  eg_tab$dat_grp[ii]
  
  tmpsumm_all_for_binplt <- comb_df %>%   filter(dat_grp==eg_tab$dat_grp[ii] & 
                                                   esutype==eg_tab$esutype[ii] & 
                                                   reartype==eg_tab$reartype[ii] &
                                                   bin %in% my_bins)
  tmpsumm_all_for_binplt$bin=factor(as.character(tmpsumm_all_for_binplt$bin))
  
  comb_df$empty_cells=apply(comb_df[,c("n.11","n.10","n.01","n.00")],1,function(x){length(which(x==0))})
  tmpsumm_all_for_binplt$too_sparse=tmpsumm_all_for_binplt$empty_cells>0
  # ggplot(data=tmpsumm_all_for_binplt) + geom_errorbar() + facet_wrap(~bin)
  
  # tmpsumm_all_for_binplt$defin_det_yr <- factor(tmpsumm_all_for_binplt$defin_det_yr,levels=as.character(2025:1998))
  tmpsumm_all_for_binplt$s1_alt <- tmpsumm_all_for_binplt$s1
  tmpsumm_all_for_binplt$s1_alt=ifelse(is.na(tmpsumm_all_for_binplt$s1),-0.25,tmpsumm_all_for_binplt$s1)
  
  # labelExample <- c("Daily","1/2 Weekly","Weekly","Two weeks","Monthly","Annual")
  # names(labelExample) <- c("day","days3.5","week1","weeks2","month","year") # The names are the values in the df
  
  labelExample <- c("Daily","1/2 Weekly","Weekly","Two weeks","Monthly")
  names(labelExample) <- c("day","days3.5","week1","weeks2","month") # The names are the values in the df
  
  
  ############ #
  # tile map
  ############ #
  
  s1_bin_cell_plt <- ggplot() + 
    geom_tile(data=tmpsumm_all_for_binplt ,aes(y=defin_det_yr,x=binID,fill=s1_alt),color="black") +
    geom_tile(data=tmpsumm_all_for_binplt %>% filter(s1_alt>=admiss_thrsh) ,aes(y=defin_det_yr,x=binID),fill="purple",color="black") +
    # scale_fill_gradient2(low="red",mid = "yellow", high="green4") +
    scale_fill_gradient2(low="red",mid = "yellow", high="green4",limit=c(0,1),midpoint = 0.5) +
    facet_wrap(~bin,scales = "free_x",ncol=5,labeller = labeller(bin = labelExample)) + theme_minimal() + 
    labs(y="Migration year",fill=expression(S[MCN-BON])) + theme(panel.border = element_rect(fill=NA,color="black"))
  s1_bin_cell_plt
  
  tmpsumm_all_for_binplt$s1_alt <- tmpsumm_all_for_binplt$s1
  
  # annual survivals based on days with with estimates
  all_summ <- tmpsumm_all_for_binplt %>%
    group_by(bin,defin_det_yr) %>%
    summarize(NAs=sum(is.na(s1)),
              ntot=length(s1),
              annual_n=sum(N))
  tmp3 <- tmpsumm_all_for_binplt %>% left_join(all_summ %>% select(defin_det_yr,bin,annual_n)) %>%
    mutate(cnt_wt=N/annual_n) %>% filter(s1_alt>0 & is.finite(s1_alt) & s1_alt<admiss_thrsh)
  
  tmp4 <- tmp3 %>% 
    group_by(bin,defin_det_yr) %>%
    summarize(non_missing_wt_sum=sum(cnt_wt))
  tmp5 <- tmp3 %>% left_join(tmp4) %>% mutate(wt_mod=cnt_wt/non_missing_wt_sum)
  tmp5$code=paste(tmp5$defin_det_yr,tmp5$bin)
  
  est_summ <- tmp5 %>% 
    group_by(bin,defin_det_yr) %>%
    summarize(arith_mean=mean(s1),
              n=length(s1),
              sum(wt_mod), # making it so that weights sum to 1
              wt_avgS1=sum(s1*wt_mod,na.rm = T),
              wt_avgS1_VAR=sum((wt_mod^2)*(s1SE^2),na.rm = T),
              wt_avgS1_SE=sqrt(wt_avgS1_VAR),
              wt_avg_lcl=wt_avgS1-wt_avgS1_SE*1.96,
              wt_avg_ucl=wt_avgS1+wt_avgS1_SE*1.96
              # ifelse choices for placing estimates
              # wt_avg_lcl=ifelse(wt_avgS1-wt_avgS1_SE*1.96<0,0,wt_avgS1-wt_avgS1_SE*1.96),
              # wt_avg_ucl=ifelse(wt_avgS1+wt_avgS1_SE*1.96>admiss_thrsh,
              #                   max(wt_avgS1,admiss_thrsh),wt_avgS1+wt_avgS1_SE*1.96)
              
    )
  
  # est_summ$defin_det_yr <- as.numeric(as.character(est_summ$defin_det_yr))
  # est_summ$defin_det_yr <- as.numeric(as.character(est_summ$defin_det_yr))
  s1_ann_est_plt_VERT  <- ggplot() + 
    geom_errorbar(data=est_summ,aes(x=defin_det_yr,ymin = wt_avg_lcl,ymax = wt_avg_ucl),width=0) +
    # geom_errorbar(data=est_summ %>% filter(wt_avgS1>1),aes(x=defin_det_yr,ymin = wt_avg_lcl,ymax = wt_avg_ucl),width=1) +
    geom_point(data=est_summ,aes(y=wt_avgS1,x=defin_det_yr,fill=wt_avgS1),shape=21,size=3) +
    facet_wrap(~bin,ncol=1,labeller = labeller(bin = labelExample)) + 
    scale_fill_gradient2(low="red",mid = "yellow", high="green4",limit=c(0,1),midpoint = 0.5) + 
    # facet_wrap(~bin,scales = "free_x",ncol=4,labeller = labeller(bin = labelExample)) + 
    theme_minimal() + 
    labs(y=expression(S[MCN-BON]),x="Migration year",fill=expression(S[MCN-BON])) + 
    theme(panel.border = element_rect(fill=NA,color="black")) +
    geom_hline(yintercept = 0:1,linetype="dotted") #+ theme_minimal()
  
  
  plt_nm=paste(unlist(eg_tab[ii,]),collapse="_")
  
  
  png(filename = paste0("pngs/temp_and_est_plts/",plt_nm,".png"),width=16,height=9,res=600,units="in")
  grid.arrange(
    s1_bin_cell_plt,
    s1_ann_est_plt_VERT + ggtitle(plt_nm)
    ,ncol=2)
  dev.off()
  
}


# ii=4:5








