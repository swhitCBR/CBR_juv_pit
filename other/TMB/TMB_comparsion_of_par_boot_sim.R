lgr_est_outDF2 <- readRDS("est_files/lgr_est_outDF2.rds")

head(lgr_est_outDF2)
subset(lgr_est_outDF2,grp_code=="lgr_det SR_Ch1 H 2006" & bin=="year")

my_ls <- readRDS("other/TMB/my_ls.rds")

# extras
# my_ls2 <- readRDS("other/TMB/my_ls2.rds")
# my_ls3 <- readRDS("other/TMB/my_ls3.rds")
# my_lsDF <- do.call(rbind,c(my_ls,my_ls2,my_ls3))
# 
# my_lsDF <- do.call(rbind,readRDS("other/TMB/my_ls_logit.rds"))
# 
# my_lsDF$P2 <- plogis(my_lsDF$P2)
# my_lsDF$S1 <- plogis(my_lsDF$S1)
# my_lsDF$Lambda <- plogis(my_lsDF$Lambda )
# 
# 
# my_lsDF <- do.call(rbind,readRDS("other/TMB/my_ls_logS_logit_plam.rds"))
# my_lsDF$P2 <- plogis(my_lsDF$P2)
# my_lsDF$S1 <- exp(my_lsDF$S1)
# my_lsDF$Lambda <- plogis(my_lsDF$Lambda )


my_lsDF_raw <- do.call(rbind,readRDS("other/TMB/my_ls.rds"))
my_lsDF_logit <- do.call(rbind,readRDS("other/TMB/my_ls_logit.rds"))
my_lsDF_logit$P2 <- plogis(my_lsDF_logit$P2)
my_lsDF_logit$S1 <- plogis(my_lsDF_logit$S1)
my_lsDF_logit$Lambda <- plogis(my_lsDF_logit$Lambda )

my_lsDF_burn <- do.call(rbind,readRDS("other/TMB/my_ls_logS_logit_plam.rds"))
my_lsDF_burn$P2 <- plogis(my_lsDF_burn$P2)
my_lsDF_burn$S1 <- exp(my_lsDF_burn$S1)
my_lsDF_burn$Lambda <- plogis(my_lsDF_burn$Lambda )


my_lsDF <- data.frame(
  rbind(
    data.frame(trans="raw",my_lsDF_raw),
    data.frame(trans="logit",my_lsDF_logit),
    data.frame(trans="burn",my_lsDF_burn)))

table(my_lsDF_raw$n11==0)
table(my_lsDF_raw$n11==1)





 
# -1*dmultinom(
#   x=nvec,
#   prob=c(
#     p11,
#     p10,
#     p01,
#     p00),log = T)
# 
# -dmultinom()
# hist(my_lsDF$nll)
# lik_fn(nvec=3,426,64,9531,pars = 0.9558)


ggplot(data=my_lsDF_raw,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled(contour_var = "ndensity",bins=20) + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(my_lsDF_raw$P2,na.rm=T),
             y=median(my_lsDF_raw$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF_raw$P2,na.rm=T),
             y=mean(my_lsDF_raw$S1,na.rm=T),shape=21,color="blue")  #+

ggplot(data=my_lsDF_logit,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled(contour_var = "ndensity",bins=20) + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(my_lsDF_logit$P2,na.rm=T),
             y=median(my_lsDF_logit$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF_logit$P2,na.rm=T),
             y=mean(my_lsDF_logit$S1,na.rm=T),shape=21,color="blue")  #+

ggplot(data=my_lsDF_burn,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled(contour_var = "ndensity",bins=20) + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(my_lsDF_burn$P2,na.rm=T),
             y=median(my_lsDF_burn$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF_burn$P2,na.rm=T),
             y=mean(my_lsDF_burn$S1,na.rm=T),shape=21,color="blue")  #+
  



my_lsDF_burn <- do.call(rbind,c(
                        readRDS("other/TMB/my_ls_logS_logit_plam_frstrw1k2.rds"),
                        readRDS("other/TMB/my_ls_logS_logit_plam_frstrw1k3.rds"),
                        readRDS("other/TMB/my_ls_logS_logit_plam_frstrw1k4.rds"),
                        readRDS("other/TMB/my_ls_logS_logit_plam_frstrw1k5.rds")))
my_lsDF_burn$P2 <- plogis(my_lsDF_burn$P2)
my_lsDF_burn$S1 <- exp(my_lsDF_burn$S1)
my_lsDF_burn$Lambda <- plogis(my_lsDF_burn$Lambda )


ggplot(data=my_lsDF_burn,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled(contour_var = "ndensity",bins=25) + 
  geom_point(shape=1,alpha=0.1,size=0.2,color="black") +
  geom_vline(xintercept = 0.04477612)+ geom_hline(yintercept = 0.9558061) + 
  geom_point(x=median(my_lsDF_burn$P2,na.rm=T),
             y=median(my_lsDF_burn$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF_burn$P2,na.rm=T),
             y=mean(my_lsDF_burn$S1,na.rm=T),shape=21,color="blue")  #+







cbind(
c(
mean(my_lsDF_raw$Lambda-Lambda,na.rm = T),
mean(my_lsDF_raw$S1-S1,na.rm = T),
mean(my_lsDF_raw$P2-P2,na.rm = T)
),
c(
mean(my_lsDF_logit$Lambda-Lambda,na.rm = T),
mean(my_lsDF_logit$S1-S1,na.rm = T),
mean(my_lsDF_logit$P2-P2,na.rm = T)
),
c(
mean(my_lsDF_burn$Lambda-Lambda,na.rm = T),
mean(my_lsDF_burn$S1-S1,na.rm = T),
mean(my_lsDF_burn$P2-P2,na.rm = T)
))

# facet_wrap(~trans,scales="free")




library(ggplot2)
ggplot(data=my_lsDF,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled() + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(my_lsDF$P2,na.rm=T),
             y=median(my_lsDF$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF$P2,na.rm=T),
             y=mean(my_lsDF$S1,na.rm=T),shape=21,color="blue")  +
  facet_wrap(~trans,scales="free")





# my_lsDF$
# saveRDS(my_ls,"my_ls2.rds")

V <- 10000
S1 <- 0.95
P2 <- 0.05
Lambda <- 0.01

pihat <- c(
  "p11" = S1*P2*Lambda,
  "p10" =  S1*P2*(1-Lambda),
  "p01" =  S1*(1-P2)*Lambda,
  "p00" = (1-S1) + S1*(1-P2)*(1-Lambda))


# pihat <- c(
library(dplyr)
my_lsDF <- my_lsDF %>% mutate(
  V=V,
  Tp11 = S1*P2*Lambda,
  Tp10 =  S1*P2*(1-Lambda),
  Tp01 =  S1*(1-P2)*Lambda,
  Tp00 = (1-S1) + S1*(1-P2)*(1-Lambda),
  En11 = V*Tp11,
  En10 = V*Tp10,
  En01 = V*Tp01,
  En00 = V*Tp00)

library(ggplot2)

tmp <- my_lsDF[1,]

source("C:/repos/CBR_juv_pit/other/TMB/per2_SURPH_ests.R")
# per2_surph_ests(my_lsDF[1:2,1:4])

# cell_vals <- c(my_lsDF$n.11[1],my_lsDF$n.10[1],my_lsDF$n.01[1],my_lsDF$n.00[1])
cell_vals <- my_lsDF[1,1:4]
names(cell_vals) <-  c("n.11","n.10","n.01","n.00")
SURPH_out <- per2_surph_ests(cell_vals,w_table = F)
SURPH_out


head(my_lsDF)


# source("R/CJS_aggre_fxns.R")
# source("R/CJS_aggre_est_2per.R")
# source("R/get_mom_mat.R")
# CJS_aggre_fxns(dat_in=my_lsDF[,1:4],DH_cols="DH_red")


# bias adjustments Burnham et al. 1987 pg208-9


# sim_est <- do.call(rbind,my_ls)
par(mfrow=c(1,3))
hist(my_lsDF[,5],main="P2"); abline(v=P2,col=2,lwd=2)
hist(my_lsDF[,6],main="S1"); abline(v=S1,col=2,lwd=2)
hist(my_lsDF[,7],main="Lambda"); abline(v=Lambda,col=2,lwd=2)

subset(my_lsDF,S1>3)

mean(my_lsDF$Lambda-Lambda,na.rm = T)
mean(my_lsDF$S1-S1,na.rm = T)
mean(my_lsDF$P2-P2,na.rm = T)

# aa=my_lsDF$S1-S1
hist(aa)

# hist(my_lsDF$S1-S1,na.rm = T)

# cell_valsTAB <- my_lsDF[1:3,1:4]
# SURPH_out <- per2_surph_ests(cell_valsTAB,w_table=T)


# A <- (tmp$n11)/(tmp$n11+tmp$n01)
# B <- (tmp$n11+tmp$n10)/(tmp$n11+tmp$n10+tmp$n01)
# 
# B/(B+(1-B)/A)
# B/(B+((1-B)/A))


library(ggplot2)
ggplot(data=my_lsDF,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled() + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(my_lsDF$P2,na.rm=T),
             y=median(my_lsDF$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(my_lsDF$P2,na.rm=T),
             y=mean(my_lsDF$S1,na.rm=T),shape=21,color="blue") 
  
  # geom_contour()





  # )





10000*pihat


