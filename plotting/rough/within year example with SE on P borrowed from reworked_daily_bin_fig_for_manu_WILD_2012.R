# source("functions.R")
library(ggplot2)
library(lubridate)
library(reshape2)
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



MCN_BON_2perDF <- readRDS("est_files/MCN_BON_per_2_mods_outDF_sep.rds" )
MCN_BON_2perDF <- MCN_BON_2perDF[,-c(which(names(MCN_BON_2perDF)=="p1")[2],which(names(MCN_BON_2perDF)=="lambda")[2])]
MCN_BON_2perDF


bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

# sus_inds=which(is.na(MCN_BON_2perDF_filt$binID))
# na_days_DF<- MCN_BON_2perDF_filt[sus_inds,]
# table(na_days_DF$bin)
# table(na_days_DF$bin)
# na_days_DF$unq_binID
# View(na_days_DF)
# na_days_DF_context <- do.call(rbind,
#                       lapply(1:length(sus_inds),
#                       function(x) {
#                         MCN_BON_2perDF_filt[(sus_inds[x]-3):sus_inds[x],]}))

head(MCN_BON_2perDF)
S_EST_THRESH = 1.1
S_CV_EST_THRESH = 50

MCN_BON_2perDF_filt <- MCN_BON_2perDF %>% filter(!is.na(binID)) %>%
  mutate(
    grt_admiss_val=s1 > S_EST_THRESH,
    s1SE=sqrt(s1_var),
    s1CV=(s1SE/s1)*100,
    abvCVthresh=s1CV>S_CV_EST_THRESH,
    no_estimate=(is.na(s1) | is.infinite(s1)),
    partial_prop=ind_bin_hours/full_bin_hours,
    inadmissCV=!no_estimate & (!grt_admiss_val) & abvCVthresh,
    inadmissBIG=!no_estimate & grt_admiss_val & (!abvCVthresh),
    inadmissEITHER=!no_estimate & (grt_admiss_val | abvCVthresh),
    inadmissBOTH=!no_estimate & (grt_admiss_val & abvCVthresh),
    BAD=no_estimate | inadmissBIG | inadmissCV,
    GOOD=!(BAD))




############################# #
# Individual data group
############################# #

table(MCN_BON_2perDF_filt$grp_code)

table(MCN_BON_2perDF_filt$defin_det_yr)

# sub
# sub_grps <-c("mcn_det SR_Ch1 H 2015",
#              "mcn_det SR_Ch1 H 2016",
#              "mcn_det SR_Ch1 H 2017",
#              "mcn_det SR_Ch1 H 2018",
#              "mcn_det SR_Ch1 H 2019","mcn_det SR_Ch1 H 2020","mcn_det SR_Ch1 H 2021")

sub <- MCN_BON_2perDF_filtbinsub <- MCN_BON_2perDF_filt %>% 
  filter(dat_grp=="mcn_det" & defin_det_yr %in% c(2010:2023) & esutype=="SR_Ch1", reartype=="W",
         bin %in% c("day","week1","weeks2","month","year")) %>%
  mutate(bin=factor(bin,rev(c("day","days3.5","week1","weeks2","month","year")))) %>%
  group_by(dat_grp,esutype,reartype,defin_det_yr) %>%
  mutate(n_days=as.numeric(difftime(max(bin_end),min(bin_strt),"days")))
sub$day_binend=sub$bin_end

sub_grps <- unique(sub$grp_code)


# bin attributes table
bin_tab_sub <- bin_tab_ls_combDFwYR %>% filter(grp_code %in% sub_grps,
                                               bin %in% c("day","week1","weeks2","days3.5","month"))


out_ls=list()
for(ii in 1: length(unique(bin_tab_sub$grp_code))){
  day_sub=bin_tab_sub %>% filter(bin=="day" &grp_code==unique(bin_tab_sub$grp_code)[ii]) %>% mutate(day_int=interval(bin_strt,bin_end))
  
  mnth_sub=bin_tab_sub %>% filter(bin=="month" &grp_code==unique(bin_tab_sub$grp_code)[ii])%>% mutate(month_int=interval(bin_strt,bin_end))
  week1_sub=bin_tab_sub %>% filter(bin=="week1" &grp_code==unique(bin_tab_sub$grp_code)[ii])%>% mutate(week_int=interval(bin_strt,bin_end))
  week2_sub=bin_tab_sub %>% filter(bin=="weeks2" &grp_code==unique(bin_tab_sub$grp_code)[ii])%>% mutate(weeks2_int=interval(bin_strt,bin_end))
  
  out_ls[[ii]]=data.frame(day_sub[,c(1:5)],
                          month_bin=which(sapply(mnth_sub$month_int,function(x) day_sub$day_int %within% x),arr.ind = T)[,2],
                          week1_bin=which(sapply(week1_sub$week_int,function(x) day_sub$day_int %within% x),arr.ind = T)[,2],
                          weeks2_bin=which(sapply(week2_sub$weeks2_int,function(x) day_sub$day_int %within% x),arr.ind = T)[,2])
}

bin_code_key <- do.call(rbind,out_ls)

# table(is.na(sub$binID))
# sub[(which(is.na(sub$binID)):,]
# sub[(which(is.na(sub$binID))[1]-3):which(is.na(sub$binID))[1],]
# lapply(1:length(which(is.na(sub$binID))),function(x) sub[(which(is.na(sub$binID))[x]-3):which(is.na(sub$binID))[x],])






# sub_nondays <- sub[sub$bin!="day",]  %>% left_join(bin_code_key)

sub_day <- sub[sub$bin=="day",] %>% rename(day_binID=binID,bin_code_day=bin_code)  %>% mutate(N_daily=N)

sub_mnth <- sub[sub$bin=="month",]  %>% left_join(bin_code_key)
sub_week <- sub[sub$bin=="week1",]  %>% left_join(bin_code_key)
sub_weeks2 <- sub[sub$bin=="weeks2",]  %>% left_join(bin_code_key)# head(data.frame(sub_mnth))

mnths_by_day <- sub[sub$bin=="day",]  %>% 
  left_join(bin_code_key) %>%
  rename(day_binID=binID,bin_code_day=bin_code) %>% 
  mutate(bin_code=paste(dat_grp,esutype,reartype,defin_det_yr,"month",month_bin),bin="month") %>%
  select(grp,RT_COMB,aggre_lev,dat_grp,esutype,reartype,defin_det_yr,grp_code,bin,bin_code,day_binID)  %>%
  left_join(sub_mnth,by = "bin_code") %>%
  rename(bin=bin.y,grp_code=grp_code.y,defin_det_yr=defin_det_yr.x,dat_grp=dat_grp.x)

weeks_by_day <- sub[sub$bin=="day",]  %>% 
  left_join(bin_code_key) %>%
  rename(day_binID=binID,bin_code_day=bin_code) %>% 
  mutate(bin_code=paste(dat_grp,esutype,reartype,defin_det_yr,"week1",week1_bin),bin="week") %>%
  select(grp,RT_COMB,aggre_lev,dat_grp,esutype,reartype,defin_det_yr,grp_code,bin,bin_code,day_binID)  %>%
  left_join(sub_week,by = "bin_code") %>%
  rename(bin=bin.y,grp_code=grp_code.y,defin_det_yr=defin_det_yr.x,dat_grp=dat_grp.x)


weeks2_by_day <- sub[sub$bin=="day",]  %>% 
  left_join(bin_code_key) %>%
  rename(day_binID=binID,bin_code_day=bin_code) %>% 
  mutate(bin_code=paste(dat_grp,esutype,reartype,defin_det_yr,"weeks2",weeks2_bin),bin="weeks2") %>%
  select(grp,RT_COMB,aggre_lev,dat_grp,esutype,reartype,defin_det_yr,grp_code,bin,bin_code,day_binID)  %>%
  left_join(sub_weeks2,by = "bin_code") %>%
  rename(bin=bin.y,grp_code=grp_code.y,defin_det_yr=defin_det_yr.x,dat_grp=dat_grp.x)


sub_year <- sub[sub$bin=="year",]  %>% left_join(bin_code_key)

year_by_day <- sub_day %>% 
  group_by(grp,RT_COMB,aggre_lev,dat_grp,esutype,reartype,defin_det_yr,grp_code,bin,day_binID) %>%
  mutate(bin="year",binID=as.numeric(defin_det_yr)-min(as.numeric(defin_det_yr))+1)
year_by_dayDF <- data.frame(year_by_day[,c("day_binID")],sub_year[match(year_by_day$grp_code,sub_year$grp_code),])




tmp <- bind_rows(
  sub_day, 
  mnths_by_day,
  weeks_by_day,
  weeks2_by_day,
  year_by_dayDF) %>% 
  mutate(bin=factor(bin,c("day","week1","weeks2","month","year"))) 

# tmp <- bind_rows(
#   sub_day, 
#   mnths_by_day,
#   weeks_by_day,
#   weeks2_by_day#,
#   # year_by_dayDF
# ) %>% 
#   mutate(bin=factor(bin,c("day","week1","weeks2","month"))) 
# 



tmp$code4 <- paste(tmp$grp_code,tmp$day_binID)
sub_day$code4 <- paste(sub_day$grp_code,sub_day$day_binID)
tmp$N_daily <- sub_day$N[match(tmp$code4,sub_day$code4)]
tmp$day_binend <- sub_day$bin_end[match(tmp$code4,sub_day$code4)]
tmp$binID <- ifelse(tmp$bin=="day",tmp$day_binID,tmp$binID)

tmp$category=NA
# tmp$category[tmp$inadmissCV]="Inadmissable CV>50%"
# tmp$category[tmp$inadmissEITHER]="Inadmissable\n(CV >50% OR S>1.1)"
# tmp$category[tmp$GOOD]="Estimable"
tmp$category[!tmp$no_estimate]="Estimable"
tmp$category[tmp$no_estimate]="Too Sparse"
# tmp$category[tmp$inadmissBIG]="Inadmissable S>1.1"


tmp$category[!is.na(tmp$s1) & is.finite(tmp$s1) &  tmp$s1>1.1]="Inadmissable S>1.1"


tmp$category[tmp$s1<=1.1 & !is.na(tmp$s1<=1.1)]="Estimable"





binendsDF <- sub %>% select(bin,grp_code,day_binend,official_strt,official_end)
binendsDF$N_daily <- sub_day$N_daily[match(binendsDF$day_binend,sub_day$bin_end)+1]
binendsrugDF <- binendsDF %>% group_by(bin,grp_code) #%>%
# filter(!(bin=="day" & (!day_binend %in% c(min(day_binend),max(day_binend)))))
binstrtDF <- sub %>% select(bin,grp_code,official_strt,official_end,bin_strt,binID) %>% 
  filter(binID==1)   

binendsrugDF <- binendsrugDF %>% filter(bin!="year")
binendsDF <- binendsDF %>% filter(bin!="year")

ggplot() + geom_bar(stat="identity",data=tmp,
                    aes(fill=category,x=day_binend,y=N_daily))+#,color="black") +
  facet_grid(bin~defin_det_yr,scale="free") +
  scale_x_datetime(date_labels = "%B",date_breaks = "1 month") +
  scale_fill_manual(values=alpha(c("green3","orange2","red3"),0.6))+
  # scale_fill_manual(values=alpha(c("black","gray","white"),0.6))+
  geom_point(data=binendsDF,aes(x=official_strt,y=0),pch=21) +
  geom_point(data=binendsDF,aes(x=official_end,y=0),pch=21) +
  geom_segment(data=binendsDF[binendsDF$bin!="day",],aes(x=day_binend,xend=day_binend,y=0,yend=N_daily)) + #,sides = "top",length = unit(1,"lines")) +
  geom_rug(data=binstrtDF,aes(x=bin_strt),sides = "top") +
  geom_rug(data=binendsrugDF,aes(x=day_binend),sides = "top") +
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  expand_limits( y = c(0,max(tmp$N_daily)*1.2))+
  labs(x="",y="Unique Tag Detections")
# 


day_codes <- c(
  `day` = "1",
  `week1` = "7",
  `weeks2` = "14",
  `month` = "28"
)


binendsrugDF_shift <- binendsrugDF
binendsrugDF_shift$day_binend <- binendsrugDF_shift$day_binend + lubridate::hours(12)

binstrtDF_shift <- binstrtDF
binstrtDF_shift$bin_strt <- binstrtDF_shift$bin_strt + lubridate::hours(12)

tmp$s1 <- ifelse(is.infinite(tmp$s1),NA,tmp$s1)

tmp12summ <- tmp[tmp$defin_det_yr==2012,] %>% #filter(bin!="day") %>% 
  group_by(bin,category,s1,s1SE,p1,p1SE) %>% 
  summarize(day_binID_mean=ifelse(bin=="day",first(day_binID),mean(day_binID)),
            day_binend=mean(day_binend),
            max_N_count=max(N_daily))

tmp12summ$N_daily=tmp12summ$s1*max(tmp12summ$max_N_count)*2
tmp12summ$N_daily_lw=(tmp12summ$s1-tmp12summ$s1SE)*max(tmp12summ$max_N_count)*2
tmp12summ$N_daily_hi=(tmp12summ$s1+tmp12summ$s1SE)*max(tmp12summ$max_N_count)*2

tmp12summ$s1_lw=(tmp12summ$s1-tmp12summ$s1SE)
tmp12summ$s1_hi=(tmp12summ$s1+tmp12summ$s1SE)
table(tmp$category)


tmp12summ$s1_lw=(tmp12summ$s1-tmp12summ$s1SE)
tmp12summ$s1_hi=(tmp12summ$s1+tmp12summ$s1SE)

tmp12summ$p1_lw=(tmp12summ$p1-tmp12summ$p1SE)
tmp12summ$p1_hi=(tmp12summ$p1+tmp12summ$p1SE)



table(tmp$category,tmp$s1>1.1)

g_dbar=ggplot() + geom_bar(stat="identity",data=tmp[tmp$defin_det_yr==2012,],
                           aes(fill=category,x=day_binend,y=N_daily),color="black") +
  # geom_errorbar(data=tmp12summ,aes(x=day_binend,y=s1,ymin=N_daily_lw,ymax=N_daily_hi),width=0) +
  # geom_point(data=tmp12summ,aes(x=day_binend,y=N_daily),size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes)) +
  scale_fill_manual(values=c("gray30","gray70","white"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  expand_limits( y = c(0,max(tmp[tmp$defin_det_yr==2012,]$N_daily)*1.2))+
  # labs(x="") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        # axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())#+ theme(str) +

table(is.na(tmp12summ[tmp12summ$s1<=1.1,]$bin))

gpt_SURV <- ggplot() + 
  geom_errorbar(data=tmp12summ[!is.na(tmp12summ$s1) & tmp12summ$s1<=1.1,],aes(x=day_binend,ymin=s1_lw,ymax=s1_hi),width=0) +
  geom_point(data=tmp12summ,aes(x=day_binend,y=s1,fill=tmp12summ$s1<=1.1),shape=21,size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes))+#,scales = "free_y") +
  scale_fill_manual(values=c("gray70","gray30"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  geom_hline(yintercept = 1,linetype = "dotted",color="gray") +
  labs(x="") + ggtitle("dfsdf") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        legend.position="none")#+ theme(str) +



gpt_DET <- ggplot() + 
  geom_errorbar(data=tmp12summ[!is.na(tmp12summ$s1) & tmp12summ$s1<=1.1,],aes(x=day_binend,ymin=p1_lw,ymax=p1_hi),width=0) +
  geom_point(data=tmp12summ,aes(x=day_binend,y=p1,fill=tmp12summ$p1<=1.1),shape=21,size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes))+#,scales = "free_y") +
  scale_fill_manual(values=c("gray70","gray30"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  geom_hline(yintercept = 1,linetype = "dotted",color="gray") +
  labs(x="") + ggtitle("dfsdf") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        legend.position="none")#+ theme(str) +



library(gridExtra)
library(ggplot2)
# install.packages("gridExtra")

# png(filename = "temp/daily_binning_plt_WILD.png",width=10,height=8,res=600,units="in")
grid.arrange(g_dbar+ggtitle("Daily tag detections (LGR)_WILD"),
             gpt_DET + ggtitle("Detection Estimate (MCN)_WILD"),
             gpt_SURV + ggtitle("Suvival Estimate (MCN-BON)_WILD"),ncol=3)
# dev.off()













g_dbar=ggplot() + geom_bar(stat="identity",data=tmp[tmp$defin_det_yr==2012,],
                           aes(fill=category,x=day_binend,y=N_daily),color="black") +
  # geom_errorbar(data=tmp12summ,aes(x=day_binend,y=s1,ymin=N_daily_lw,ymax=N_daily_hi),width=0) +
  # geom_point(data=tmp12summ,aes(x=day_binend,y=N_daily),size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes)) +
  scale_fill_manual(values=c("gray30","gray70","white"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  expand_limits( y = c(0,max(tmp[tmp$defin_det_yr==2012,]$N_daily)*1.2))+
  # labs(x="") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        # axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())#+ theme(str) +

table(is.na(tmp12summ[tmp12summ$s1<=1.1,]$bin))

gpt_SURV <- ggplot() + 
  geom_errorbar(data=tmp12summ[!is.na(tmp12summ$s1) & tmp12summ$s1<=1.1,],aes(x=day_binend,ymin=s1_lw,ymax=s1_hi),width=0) +
  geom_point(data=tmp12summ,aes(x=day_binend,y=s1,fill=tmp12summ$s1<=1.1),shape=21,size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes))+#,scales = "free_y") +
  scale_fill_manual(values=c("gray70","gray30"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  geom_hline(yintercept = 1,linetype = "dotted",color="gray") +
  labs(x="") + ggtitle("dfsdf") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        legend.position="none")#+ theme(str) +



gpt_DET <- ggplot() + 
  geom_errorbar(data=tmp12summ[!is.na(tmp12summ$s1) & tmp12summ$s1<=1.1,],aes(x=day_binend,ymin=p1_lw,ymax=p1_hi),width=0) +
  geom_point(data=tmp12summ,aes(x=day_binend,y=p1,fill=tmp12summ$p1<=1.1),shape=21,size=3) +
  facet_wrap(~bin,ncol=1,strip.position="right",labeller = as_labeller(day_codes))+#,scales = "free_y") +
  scale_fill_manual(values=c("gray70","gray30"))+
  geom_rug(data=binstrtDF_shift[binstrtDF_shift$bin !="day" & binstrtDF_shift$defin_det_yr==2012,],aes(x=bin_strt),sides = "top",length=unit(10,"cm")) +
  geom_rug(data=binendsrugDF_shift[binendsrugDF_shift$bin !="day" & binendsrugDF_shift$defin_det_yr==2012,],
           aes(x=day_binend),sides = "top",length=unit(10,"cm")) +
  mytheme + theme(panel.spacing.y =unit(1.5, "lines"))  +
  geom_hline(yintercept = 1,linetype = "dotted",color="gray") +
  labs(x="") + ggtitle("dfsdf") +#+ theme(str)
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.title.y=element_blank(),
        legend.position="none")#+ theme(str) +



library(gridExtra)
library(ggplot2)
# install.packages("gridExtra")

# png(filename = "temp/daily_binning_plt_WILD.png",width=10,height=8,res=600,units="in")
grid.arrange(g_dbar+ggtitle("Daily tag detections (LGR)_WILD"),
             gpt_DET + ggtitle("Detection Estimate (MCN)_WILD"),
             gpt_SURV + ggtitle("Suvival Estimate (MCN-BON)_WILD"),ncol=3)
# dev.off()









# png(filename = "temp/daily_binning_plt.png",width=10,height=8,res=600,units="in")
# grid.arrange(g_dbar+ggtitle("Daily tag detections (LGR)"),gpt + ggtitle("Suvival Estimate (MCN-BON)"),ncol=2)
# dev.off()

