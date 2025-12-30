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



library(magick)

bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

# mcn_est_outDF3 <- readRDS("est_files/mcn_est_outDF3.rds")
# lgr_est_outDF3 <- readRDS("est_files/lgr_est_outDF3.rds")

# source("R/get_ests_from_MSS.R")
# 
# # datasets with bins capture histories etc
# mcn_est_outDF3 <- data.frame(mcn_est_outDF2,t(sapply(1:nrow(mcn_est_outDF2),function(x) per2_surph_ests(cell_vals_in = unlist(mcn_est_outDF2[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))
# lgr_est_outDF3 <- data.frame(lgr_est_outDF2,t(sapply(1:nrow(lgr_est_outDF2),function(x) per2_surph_ests(cell_vals_in = unlist(lgr_est_outDF2[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))



comb_df <- readRDS("est_files/MCN_BON_per_2_mods_outDF_sep.rds")

head(comb_df)

# comb_df <- readRDS("est_files/MCN_BON_per_2_mods_outDF_sep.rds")
# getting rid of duplicate columns
# comb_df <- comb_df[,-c(which(names(comb_df)=="lambda")[2],which(names(comb_df)=="p1")[2])]
ii=4

###################### #
# inputs to function
###################### #

admiss_thrsh=1.1
eg_tab <- expand.grid(
  dat_grp=c("mcn_det","lgr_det","lgr_pooled"),
  esutype=c("SR_Sthd","SR_Ch1","SR_Sock"),
  reartype=c("W","H"))

my_bins=c("day","days3.5","week1","weeks2","month","year")

my_bins=c("day","days3.5","week1","weeks2","month")

ii=4
ii=1
eg_tab[ii,]
eg_tab <- eg_tab[c(1,2,4,5,10,11,13,14),]

# save.image("tmp_img.Rdata")
# load("tmp_img.Rdata")

# my_bins=c("day","days3.5","week1","weeks2","month","year")
my_bins=c("day","week1","weeks2","month")
my_bins=c("day","week1","month")

ii=1

# for(ii in 1:nrow(eg_tab)){
print(ii)
# for(ii in 1:3){

# y-axis expression label
if(eg_tab$dat_grp[ii]=="mcn_det"){
  my_exp <- expression(S[MCN-BON])
} else{
  my_exp <- expression(S[LGR-MCN])
} 

eg_tab$dat_grp[ii]

comb_df$empty_cells=apply(comb_df[,c("n.11","n.10","n.01","n.00")],1,function(x){length(which(x==0))})
tmpsumm_all_for_binplt <- comb_df %>%   filter(dat_grp==eg_tab$dat_grp[ii] & 
                                                 esutype==eg_tab$esutype[ii] & 
                                                 reartype==eg_tab$reartype[ii] &
                                                 bin %in% my_bins)
tmpsumm_all_for_binplt$bin=factor(as.character(tmpsumm_all_for_binplt$bin))


tmpsumm_all_for_binplt$too_sparse=tmpsumm_all_for_binplt$empty_cells>0

tmpsumm_all_for_binplt$defin_det_yr <- factor(tmpsumm_all_for_binplt$defin_det_yr,levels=as.character(2025:1998))
tmpsumm_all_for_binplt$s1_alt <- tmpsumm_all_for_binplt$s1
tmpsumm_all_for_binplt$s1_alt=ifelse(is.na(tmpsumm_all_for_binplt$s1),-0.25,tmpsumm_all_for_binplt$s1)

############ #
# tile map
############ #

tmpsumm_all_for_binplt <- tmpsumm_all_for_binplt %>% filter(bin %in% my_bins & defin_det_yr %in% as.character(1998:2025))
tmpsumm_all_for_binplt$defin_det_yr=factor(tmpsumm_all_for_binplt$defin_det_yr,as.character(2025:1998))

tmpsumm_all_for_binplt$bin=factor(as.character(tmpsumm_all_for_binplt$bin),levels =c("day","week1","month") )
labelExample <- c("Daily","Weekly","Monthly")
names(labelExample) <- c("day","week1","month") # The names are the values in the df


s1_bin_cell_plt <- ggplot() + 
  geom_tile(data=tmpsumm_all_for_binplt ,aes(y=defin_det_yr,x=binID,fill=s1_alt),color="black") +
  geom_tile(data=tmpsumm_all_for_binplt %>% filter(s1_alt>=admiss_thrsh) ,aes(y=defin_det_yr,x=binID),fill="purple",color="black") +
  # scale_fill_gradient2(low="red",mid = "yellow", high="green4") +
  scale_fill_gradient2(low="red",mid = "yellow", high="green4",limit=c(0,1),midpoint = 0.5,na.value = "gray80") +
  facet_wrap(~bin,scales = "free_x",ncol=5,labeller = labeller(bin = labelExample)) + theme_minimal() + 
  labs(y="Migration year",x="Bin",fill=my_exp) + theme(panel.border = element_rect(fill=NA,color="black")) +
  theme(legend.position="right") +
  theme(plot.margin = margin(1,1,1,1,unit = "cm"),
        axis.title.x=element_text(size=18,vjust = -2),axis.title.y=element_blank(),axis.text=element_text(size=13,color="black"),strip.text = element_text(size=20,margin = margin(0,0,0.25,0,unit = "cm")),panel.spacing.x=unit(2, "lines"))


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


tmpsumm_all_for_day_contrib_plt <- tmpsumm_all_for_binplt %>% 
  mutate(s1_fin=ifelse(s1_alt<0 | is.infinite(s1_alt) | s1_alt>admiss_thrsh,NA,s1_alt)) %>%
  mutate(ind=1:n(),
         pool_na=is.na(s1_fin),
         defin_det_yr=as.numeric(as.character(defin_det_yr)),
         ind=ifelse(pool_na,NA,ind))


# tmpsumm_all_for_day_contrib_plt$ind_bin_days[is.na(tmpsumm_all_for_day_contrib_plt$ind_bin_days)]=c(28,7,1)
tmpsumm_all_for_day_contrib_plt$ind_bin_days[tmpsumm_all_for_day_contrib_plt$bin=="day"]=1

total_dat_days=tmpsumm_all_for_day_contrib_plt %>% 
  group_by(bin,defin_det_yr) %>%
  summarize(ind_bin_days=sum(ind_bin_days)) %>%
  mutate(s1_fin=99)

total_dat_days[is.na(total_dat_days$ind_bin_days),]


tmpsumm_all_for_contrib_plt <- tmpsumm_all_for_binplt %>% 
  mutate(s1_fin=ifelse(s1_alt<0 | is.infinite(s1_alt) | s1_alt>admiss_thrsh,NA,s1_alt)) %>%
  # arrange(bin,defin_det_yr,N,ind_bin_days ) %>%
  mutate(ind=1:n(),
         pool_na=is.na(s1_fin),
         defin_det_yr=as.numeric(as.character(defin_det_yr)),
         ind=ifelse(pool_na,NA,ind)) 

total_dat=tmpsumm_all_for_contrib_plt %>% group_by(bin,defin_det_yr) %>%
  summarize(N=sum(N)) %>%
  mutate(s1_fin=99)

purp_dat=tmpsumm_all_for_contrib_plt %>% mutate(s1_fin=ifelse(s1_fin<1.1 & is.na(s1_fin),NA,99))


contrib_plt <-   ggplot() +  
  geom_bar(data=tmpsumm_all_for_contrib_plt,aes(y=N,x=defin_det_yr,fill=s1_fin,color=s1_fin),
           position="stack",stat="identity") + 
  geom_bar(data=total_dat,aes(y=N,x=defin_det_yr),fill=NA,
           position="stack",stat="identity",color="black") + 
  facet_wrap(~bin,scales="free_x",labeller = labeller(bin = labelExample)) + labs(y="Unique tags",x="Migration year",fill=my_exp) +
  scale_fill_gradient2(low="red",mid = "yellow", high="green4",limit=c(0,1),midpoint = 0.5,na.value = "gray80") +
  scale_color_gradient2(low="black",mid = "black", high="black",limit=c(0,1),midpoint = 0.5,na.value = NA) +
  theme_minimal() + guides(color="none")+ scale_y_continuous(expand = c(0, 0, 0.05, 0))+
  theme(panel.border = element_rect(fill=NA,color="black"),legend.position="bottom",
        plot.background=element_rect(fill = "transparent", colour = NA),panel.background=element_rect(fill = "transparent", colour = NA)
  )

purp_dat <- tmpsumm_all_for_contrib_plt
purp_dat$s1_fin=NA
purp_dat$s1_fin[is.finite(purp_dat$s1_alt) & !is.na(purp_dat$s1_alt) & purp_dat$s1_alt>1.1]=99

purp_contrib_plt <-   ggplot() +  
  geom_bar(data=purp_dat,aes(y=N,x=defin_det_yr,fill=s1_fin),
           position="stack",stat="identity",color=NA) +
  geom_bar(data=total_dat,aes(y=N,x=defin_det_yr),fill=NA,
           position="stack",stat="identity",color="black") +
  facet_wrap(~bin,scales="free_x",labeller = labeller(bin = labelExample)) + labs(y="Unique tags",x="Migration year",fill=my_exp) +
  # scale_fill_manual(values=c("red"))+
  scale_fill_gradient2(low="purple",mid = "purple", high="purple",limit=c(0,100),midpoint = 50,na.value = "NA") +
  theme_minimal() + guides(color="none")+ scale_y_continuous(expand = c(0, 0, 0.05, 0))+
  theme(panel.border = element_rect(fill=NA,color="black"),legend.position="bottom",
        plot.background=element_rect(fill = "transparent", colour = NA),panel.background=element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )


png("tmp_png1.png",bg = "transparent",units = "in",width = 8,height=4.5,res = 300)
purp_contrib_plt
dev.off()

png("tmp_png2.png",bg = "transparent",units = "in",width = 8,height=4.5,res = 300)
contrib_plt
dev.off()

tmp_png1 <- image_read("tmp_png1.png")
tmp_png2 <- image_read("tmp_png2.png")
tmp_png1_crp <- image_crop(tmp_png1,"2400x1100+0")

img_v <- c(tmp_png2,tmp_png1_crp)
tmp_mos <- image_mosaic(img_v)

est_summ <- tmp5 %>% 
  group_by(bin,defin_det_yr) %>%
  summarize(arith_mean=mean(s1),
            n=length(s1),
            sum(wt_mod), # making it so that weights sum to 1
            wt_avgS1=sum(s1*wt_mod,na.rm = T),
            wt_avgS1_VAR=sum((wt_mod^2)*(s1SE^2),na.rm = T),
            wt_avgS1_SE=sqrt(wt_avgS1_VAR),
            wt_avg_lcl=wt_avgS1-wt_avgS1_SE*1.96,
            wt_avg_ucl=wt_avgS1+wt_avgS1_SE*1.96)


s1_ann_est_plt_HORIZ  <- ggplot() + 
  geom_errorbar(data=est_summ,aes(x=defin_det_yr,ymin = wt_avg_lcl,ymax = wt_avg_ucl),width=0) +
  geom_point(data=est_summ,aes(y=wt_avgS1,x=defin_det_yr,fill=wt_avgS1),shape=21,size=3) +
  facet_wrap(~bin,ncol=3,labeller = labeller(bin = labelExample)) + 
  scale_fill_gradient2(low="red",mid = "yellow", high="green4",limit=c(0,1),midpoint = 0.5) + 
  theme_minimal() + 
  labs(y=my_exp,x="Migration year",fill=my_exp) + 
  theme(panel.border = element_rect(fill=NA,color="black")) +
  geom_hline(yintercept = 0:1,linetype="dotted") + 
  theme(legend.position="none")

png("tmp_png3.png",bg = "transparent",units = "in",width = 8,height=3.5,res = 300)
s1_ann_est_plt_HORIZ
dev.off()

tmp_png3 <- image_read("tmp_png3.png")

comb_plt <- image_append(c(tmp_png3,tmp_mos),stack=T)

plt_nm=paste(unlist(eg_tab[ii,]),collapse="_")

image_write(comb_plt, paste0("pngs/",plt_nm,".png"))


# }

