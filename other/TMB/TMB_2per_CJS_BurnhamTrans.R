# # source("functions.R")
# 
# # code-level detection history with wide format bin names
# mcn_dh_tab2 <- readRDS("comp_files/mcn_dh_tab2_9825_wPD568.rds")
# mcn_dh_tab2$DH_red <- factor(paste(mcn_dh_tab2$BON,mcn_dh_tab2$Estuary),
#                              levels=c("1 1","1 0","0 1","0 0"))
# 
# # bin attributes table
# # bin_tab_ls_combDFwYR <- readRDS("temp/bin_tab_ls_combDFwYR.rds")
# 
# bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")
# 
# source("R/get_CJS_count_ls.R")
# 
# source("R/CJS_aggre_fxns.R")
# source("R/CJS_aggre_est_2per.R")
# source("R/get_mom_mat.R")
# 
# MCN_cjs_dataset_ls <- CJS_aggre_fxns(dat_in=mcn_dh_tab2,DH_cols="DH_red")
# lapply(MCN_cjs_dataset_ls$dat_ls_RTsep,dim)
# 
# 
# subset(mcn_dh_tab2[1:5,])
# 
# 
# 
# # code-level detection history with wide format bin names
# LGR_cjs_dataset_ls <- CJS_aggre_fxns(dat_in=lgr_dh_tab2,DH_cols="DH_red")
# lapply(LGR_cjs_dataset_ls$dat_ls_RTsep,dim)
# lapply(LGR_cjs_dataset_ls$dat_ls_RTcomb,dim)
# 
# LGR_all_out <- CJS_aggre_est_2per(dat_in=lgr_dh_tab2,DH_cols="DH_red")
# lgr_est_outDF <- LGR_all_out$outDF
# head(lgr_est_outDF)
# 
# lgr_est_outDF2 <- lgr_est_outDF %>%
#   mutate(bin_code=paste(grp_code,bin,binID)) %>% 
#   left_join(bin_tab_ls_combDFwYR %>% 
#               mutate(bin_code=paste(grp_code,bin,binID)))
# 
# 
# head(lgr_est_outDF2)
# 
# 
# head(LGR_all_out)
# names(LGR_all_out)
# 
# lapply(LGR_all_out,dim)
# # LGR_all_out$out_ls_RTcomb
# head(LGR_all_out$outDF)
# table(LGR_all_out$outDF$aggre_lev)
# 
# SUB <- LGR_all_out$outDF[1:3,]
# 
# SUB$swB <- SUB$n.11+SUB$n.01
# SUB$swA <- SUB$n.11+SUB$n.10
# 
# SUB$P1 <- SUB$n.11/SUB$swB
# SUB$n.11/SUB$swA
# SUB$swA/SUB$N*(1/SUB$P1)
# 
# 
# 
# SURPH_out <- per2_surph_ests(cell_vals)
# 
# 
# n.11/SUB$swA
# 
# 
# 
# cell_vals <- c(SUB$n.11[1],SUB$n.10[1],SUB$n.01[1],SUB$n.00[1])
# names(cell_vals) <-  c("n.11","n.10","n.01","n.00")
# 
# SURPH_out <- per2_surph_ests(cell_vals)
# ann_ests <- SURPH_out$Estimate
# names(ann_ests) <- c("p1","s1","lambda")
# 
# nlminb(start = c(0.8,0.06,0.05),objective = function(x){lik_fn(pars = x,nvec = cell_vals)})
# 
# optim(par = c(0.8,0.06,0.05),fn = function(x){lik_fn(pars = x,nvec = cell_vals)})

library(TMB)

setwd("C:/repos/CBR_juv_pit/other/TMB")

# TMB_data=list(
#   "cell_vals"=cell_vals)
# 
# aa=TMB_CJS_wrap()
# aa
lgr_est_outDF2 <- readRDS("C:/repos/CBR_juv_pit/est_files/lgr_est_outDF2.rds")


head(lgr_est_outDF2)

V <- lgr_est_outDF2[1,14]
S1 <- lgr_est_outDF2[1,19]
P2 <- lgr_est_outDF2[1,20]
Lambda <- lgr_est_outDF2[1,21]

# lgr_est_outDF2[1,19]
# lgr_est_outDF2[1,20]
# lgr_est_outDF2[1,21]
# 
# 
# lgr_est_outDF2[1,19]
# lgr_est_outDF2[1,20]
# lgr_est_outDF2[1,21]



cell_prob <- c("pi_11"=S1*P2*Lambda,
               "pi_10"=S1*P2*(1-Lambda),
               "pi_01"=S1*(1-P2)*Lambda,
               "pi_00"=1-S1*P2-S1*(1-P2)*Lambda
               )
sum(cell_prob)

source("C:/repos/CBR_juv_pit/other/TMB/per2_SURPH_ests.R")

n_sim <- 1000
sim_mat <- t(rmultinom(n=n_sim,prob = cell_prob,size = V))
# df_sim <- data.frame(sim_mat)
# 
# rowSums(sim_mat)


# sim_mat
saveRDS(sim_mat,"sim_mat_frstrw5.rds")
# sim_mat <- readRDS("sim_mat_frstrw.rds")

# sim_mat[1075,]
# sim_mat[1058,]
# sim_mat[1059,]
# sim_mat[1056,]
# cell_vals <- sim_mat[sample(size = 1,x = nrow(sim_mat)),]
# cell_vals <- c(8,469,94,9429)
# names(cell_vals) <-  c("n.11","n.10","n.01","n.00")

# TMB_data=list(
#   "cell_vals"=cell_vals)
# 
# aa=suppressWarnings(TMB_CJS_wrap())
# aa$out

# dyn.unload(dy)
ii=1
my_ls <- list()
# for(ii in 1:n_sim){
for(ii in 1:1000){
  
  cell_vals <- sim_mat[ii,]
  names(cell_vals) <-  c("n.11","n.10","n.01","n.00")
  
  TMB_data=list(
    "cell_vals"=cell_vals)
  
  # print(cell_vals)
  if(ii %in% seq(1,1000,100)){print(ii)}
  
  
  cond <- any(cell_vals==0)
  
  if(cond){
    my_ls[[ii]] <- data.frame(n11=cell_vals[1],n10=cell_vals[2],
                              n01=cell_vals[3],n00=cell_vals[4],
                              P2=NA,S1=NA,
                              Lambda=NA,iter=ii,zero_count=TRUE,nll=NA) 
  }
  else{
    
  # aa=suppressWarnings(TMB_CJS_wrap())
  
  aa=suppressWarnings(TMB_CJS_wrap_logS_logit_plam())
  
    
  my_ls[[ii]] <- data.frame(n11=cell_vals[1],n10=cell_vals[2],
                            n01=cell_vals[3],n00=cell_vals[4],
                            P2=aa$OPT$par[1],
                            S1=aa$OPT$par[2],
                            Lambda=aa$OPT$par[3],iter=ii,zero_count=TRUE,nll=aa$OPT$objective)
  
  my_ls[[ii]] 
  }
}


my_ls[[ii]]


saveRDS(my_ls,"my_ls_logS_logit_plam_frstrw1k5.rds")
gc()
# TMB_CJS_wrap_logS_logit_plam




# 
# using C++ compiler: ‘G__~1.EXE (GCC) 14.2.0’
# make: Nothing to be done for 'all'.
# Warning: 3 external pointers will be removed
# n.11 n.10 n.01 n.00 
# 0  452   94 9454 
# using C++ compiler: ‘G__~1.EXE (GCC) 14.2.0’
# make: Nothing to be done for 'all'.
# Error in approx(right[[2]], right[[1]], hline) : 
#   need at least two non-NA values to interpolate
# Called from: approx(right[[2]], right[[1]], hline)


my_ls[[432]]
sim_est <- do.call(rbind,my_ls)
subset(sim_est,S1>3)

subset(sim_est,S1>3)


dim(sim_est)
cbind(sim_mat[1:1000,],sim_est)

par(mfrow=c(1,3))
hist(sim_est[,5],main="P2"); abline(v=P2,col=2,lwd=2)
hist(sim_est[,6],main="S1"); abline(v=S1,col=2,lwd=2)
hist(sim_est[,7],main="Lambda"); abline(v=Lambda,col=2,lwd=2)

sim_est[sim_est$S1>3,]
sim_est[sim_est$n11==0,]
sim_est[sim_est$n10==0|sim_est$n01==0|sim_est$n11==0,]





setwd("other/TMB")
source("../../functions.R")
library(TMB)


TMB_data=list(
  "cell_vals"=c(28,500,426,10000))

cell_vals <- TMB_data$cell_vals
names(cell_vals) <-  c("n.11","n.10","n.01","n.00")






