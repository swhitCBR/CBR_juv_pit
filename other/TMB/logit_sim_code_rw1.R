library(TMB)
setwd("C:/repos/CBR_juv_pit/other/TMB")
source("C:/repos/CBR_juv_pit/other/TMB/per2_SURPH_ests.R")

lgr_est_outDF2 <- readRDS("C:/repos/CBR_juv_pit/est_files/lgr_est_outDF2.rds")
lgr_est_outDF3 <- readRDS("C:/repos/CBR_juv_pit/est_files/lgr_est_outDF3.rds")

V <- lgr_est_outDF2[1,14]
S1 <- lgr_est_outDF2[1,19]
P2 <- lgr_est_outDF2[1,20]
Lambda <- lgr_est_outDF2[1,21]

cell_prob <- c("pi_11"=S1*P2*Lambda,
               "pi_10"=S1*P2*(1-Lambda),
               "pi_01"=S1*(1-P2)*Lambda,
               "pi_00"=1-S1*P2-S1*(1-P2)*Lambda)

sum(cell_prob)
obs_cell_vals <- c(lgr_est_outDF3$n.11[1],lgr_est_outDF3$n.10[1],
                   lgr_est_outDF3$n.01[1],lgr_est_outDF3$n.00[1])

names(obs_cell_vals) <-  c("n.11","n.10","n.01","n.00")

lik_fn_burn <- function(s1,p1,lambda,nvec){
  p11<-s1*p1*lambda
  p10<-s1*p1*(1-lambda)
  p01<-s1*(1-p1)*lambda
  p00<-1-s1+s1*(1-p1)*(1-lambda)
  
  nll <- -1*dmultinom(
    x=nvec,
    prob=c(
      p11,
      p10,
      p01,
      p00),log = T)
  
  return(nll)}

opt_obs <- optim(par = c(0.8,0.06,0.05),fn = function(x){lik_fn(pars = x,nvec = obs_cell_vals)})
pars_obs <- c(plogis(opt_obs$par[1]),exp(opt_obs$par[2]),plogis(opt_obs$par[3]))

nll_obs_analytMLE <- lik_fn_burn(
  s1=S1,
  p1=P2,
  lambda=Lambda,
  nvec = obs_cell_vals)

nll_obs_numMLE <- lik_fn_burn(
  s1=pars_obs[1],
  p1=pars_obs[2],
  lambda=pars_obs[3],
  nvec = obs_cell_vals)

n_sim <- 1000
sim_mat <- t(rmultinom(n=n_sim,prob = cell_prob,size = V))

ii=1
my_ls <- list()
for(ii in 1:n_sim){
  # for(ii in 1:1000){
  cell_vals <- sim_mat[ii,]
  names(cell_vals) <-  c("n.11","n.10","n.01","n.00")
  TMB_data=list("cell_vals"=cell_vals)
  
  if(ii %in% seq(1,1000,100)){print(ii)}
  if(any(cell_vals==0)){
    my_ls[[ii]] <- data.frame(n11=cell_vals[1],n10=cell_vals[2],
                              n01=cell_vals[3],n00=cell_vals[4],
                              P2=NA,S1=NA,
                              Lambda=NA,iter=ii,zero_count=TRUE,nll=NA) 
  }
  else{
    aa=suppressWarnings(TMB_CJS_wrap_logit())
    my_ls[[ii]] <- data.frame(n11=cell_vals[1],n10=cell_vals[2],
                              n01=cell_vals[3],n00=cell_vals[4],
                              P2=aa$OPT$par[1],
                              S1=aa$OPT$par[2],
                              Lambda=aa$OPT$par[3],iter=ii,zero_count=TRUE,nll=aa$OPT$objective)
    my_ls[[ii]] }}

# my_ls[[ii]]

saveRDS(my_ls,"mylogitsim10.rds")




mylogitsim15<- rbind(
  do.call(rbind,readRDS("mylogitsim1.rds")),
  do.call(rbind,readRDS("mylogitsim2.rds")),
  do.call(rbind,readRDS("mylogitsim3.rds")),
  do.call(rbind,readRDS("mylogitsim4.rds")),
  do.call(rbind,readRDS("mylogitsim5.rds")),
  do.call(rbind,readRDS("mylogitsim6.rds")),
  do.call(rbind,readRDS("mylogitsim7.rds")),
  do.call(rbind,readRDS("mylogitsim8.rds")),
  do.call(rbind,readRDS("mylogitsim9.rds")),
  do.call(rbind,readRDS("mylogitsim10.rds")))

mylogitsim15$P2 <- plogis(mylogitsim15$P2)
mylogitsim15$S1 <- plogis(mylogitsim15$S1)
mylogitsim15$Lambda <- plogis(mylogitsim15$Lambda )

library(ggplot2)
ggplot(data=mylogitsim15,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled() + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(mylogitsim15$P2,na.rm=T),
             y=median(mylogitsim15$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(mylogitsim15$P2,na.rm=T),
             y=mean(mylogitsim15$S1,na.rm=T),shape=21,color="blue") 



ggplot(data=mylogitsim15,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled(bins=40) + 
  # geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(mylogitsim15$P2,na.rm=T),
             y=median(mylogitsim15$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(mylogitsim15$P2,na.rm=T),
             y=mean(mylogitsim15$S1,na.rm=T),shape=21,color="blue")  +
  theme(legend.position = "none")

ggplot(data=mylogitsim15,aes(x=Lambda,y=S1,color=Lambda)) + 
  geom_density_2d_filled(bins=40) + 
  # geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = Lambda)+ geom_hline(yintercept = S1) + 
  geom_point(x=median(mylogitsim15$Lambda,na.rm=T),
             y=median(mylogitsim15$S1,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(mylogitsim15$Lambda,na.rm=T),
             y=mean(mylogitsim15$S1,na.rm=T),shape=21,color="blue")  +
  theme(legend.position = "none")


ggplot(data=mylogitsim15,aes(x=Lambda,y=P2)) +#,color=Lambda)) + 
  geom_density_2d_filled(bins=40) + 
  # geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = Lambda)+
  geom_hline(yintercept = P2) + 
  geom_point(x=median(mylogitsim15$Lambda,na.rm=T),
             y=median(mylogitsim15$P2,na.rm=T),shape=21,color="red") +
  geom_point(x=mean(mylogitsim15$Lambda,na.rm=T),
             y=mean(mylogitsim15$P2,na.rm=T),shape=21,color="blue") +
  theme(legend.position = "none")






ggplot(data=mylogitsim15,aes(x=P2,color=Lambda)) + 
  geom_density()

ggplot(data=mylogitsim15,aes(x=S1,color=Lambda)) + 
  geom_density()


ggplot(data=mylogitsim15,aes(x=P2,y=S1,color=Lambda)) + 
  geom_density_2d_filled() + 
  geom_point(shape=16,alpha=0.1) +
  geom_vline(xintercept = P2)+ geom_hline(yintercept = S1) + 
  geom_point(x=pars_obs[2],
             y=pars_obs[1],shape=21,color="yellow") 


median(mylogitsim15$S1,na.rm=T)

hist(mylogitsim15$S1)
mean(mylogitsim15$S1-S1,na.rm=T)
mean(mylogitsim15$S1,na.rm=T)

hist(mylogitsim15$nll)
abline(v=nll_obs_numMLE,lwd=2,col=2)
abline(v=nll_obs_analytMLE,lwd=2,col=4)


cbind(
  c(
    mean(mylogitsim15$Lambda-Lambda,na.rm = T),
    mean(mylogitsim15$S1-S1,na.rm = T),
    mean(mylogitsim15$P2-P2,na.rm = T)
  ))


mean(mylogitsim15$nll > nll_obs_numMLE,na.rm = T)
mean(mylogitsim15$nll[1:1000] > nll_obs_numMLE,na.rm = T)

table(mylogitsim15$nll[!is.na(mylogitsim15$nll)] > nll_obs_numMLE)
