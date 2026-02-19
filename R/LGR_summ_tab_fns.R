


LGR_COMB_rt_summ1 <- function(lgr_est_tb_in,aggre_lev_in="year"){#},csv_base_nm="ann_estsTAB_rev1_",write.cvs=FALSE){
  
  # EXCLUSIVELY WITHIN-YEAR DETECTIONS AT LGR 
  ann_estsTAB_all_spp_det <- lgr_est_tb_in %>% 
    filter(aggre_lev==aggre_lev_in) %>%
    select(dat_grp,esutype,reartype,defin_det_yr,N,n.11,n.10,n.01,n.00) %>% #s1,s1_var,) %>% 
    tidyr::pivot_longer(cols = c(N,n.11,n.10,n.01,n.00))#,S1_est,S1_se))
  
  aaa=ann_estsTAB_all_spp_det %>% group_by(esutype,reartype,dat_grp,defin_det_yr,name) %>%
    summarize(value=sum(value)) %>% tidyr::pivot_wider(names_from=name)
  
  return(aaa)
  aa <- ann_estsTAB_all_spp_det %>% group_by(esutype,dat_grp,defin_det_yr,name) %>%
    summarize(value=sum(value)) %>% 
    ungroup() %>%
    tidyr::pivot_wider(names_from=dat_grp,values_from=value)
  
  return(aa)
  bb_det <- aa %>% select(-lgr_pooled) %>%
    tidyr::pivot_wider(names_from=name,values_from = lgr_det) %>%
    mutate(dat_grp="lgr_det",reartype="COMB") %>%
    relocate(esutype,reartype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)
  
  bb_pool <- aa %>% select(-lgr_det) %>% 
    tidyr::pivot_wider(names_from=name,values_from = lgr_pooled) %>%
    mutate(dat_grp="lgr_pooled",reartype="COMB") %>%
    relocate(esutype,reartype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)
  
  
  out_ls <- list("det"=bb_det,
                 "pool"=bb_pool)
  
  return(out_ls)
  
}



LGR_COMB_rt_summ <- function(lgr_est_tb_in,aggre_lev_in="year"){#},csv_base_nm="ann_estsTAB_rev1_",write.cvs=FALSE){
  
  # EXCLUSIVELY WITHIN-YEAR DETECTIONS AT LGR 
  ann_estsTAB_all_spp_det <- lgr_est_tb_in %>% 
    filter(aggre_lev==aggre_lev_in) %>%
    select(dat_grp,esutype,reartype,defin_det_yr,N,n.11,n.10,n.01,n.00) %>% #s1,s1_var,) %>% 
    tidyr::pivot_longer(cols = c(N,n.11,n.10,n.01,n.00))#,S1_est,S1_se))

  
  aa <- ann_estsTAB_all_spp_det %>% group_by(esutype,dat_grp,defin_det_yr,name) %>%
    summarize(value=sum(value)) %>% 
    ungroup() %>%
    tidyr::pivot_wider(names_from=dat_grp,values_from=value)
  
  # return(aa)
  bb_det <- aa %>% select(-lgr_pooled) %>%
    tidyr::pivot_wider(names_from=name,values_from = lgr_det) %>%
    mutate(dat_grp="lgr_det",reartype="COMB") %>%
    relocate(esutype,reartype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)
  
  bb_pool <- aa %>% select(-lgr_det) %>% 
    tidyr::pivot_wider(names_from=name,values_from = lgr_pooled) %>%
    mutate(dat_grp="lgr_pooled",reartype="COMB") %>%
    relocate(esutype,reartype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)
  
  
  out_ls <- list("det"=bb_det,
                 "pool"=bb_pool)
  
  return(out_ls)
  
}

# creating wide-format table of counts and analytical survival estimates
LGR_summ_tab_fn1 <- function(lgr_est_tb_in,csv_base_nm="ann_estsTAB_rev1_",write.cvs=FALSE){
  
  ann_estsTAB_all_spp_det <- lgr_est_tb_in %>% 
    filter(dat_grp=="lgr_det" & aggre_lev=="year") %>%
    select(dat_grp,esutype,reartype,defin_det_yr,s1,s1_var,N) %>% 
    mutate(S1_est=round(s1,3),
           S1_se=round(sqrt(s1_var),3)) %>%
    select(-s1,-s1_var) %>%
    tidyr::pivot_longer(cols = c(N,S1_est,S1_se))
  
  ann_estsTAB_all_spp_pooled <- lgr_est_tb_in %>%
    filter(dat_grp=="lgr_pooled" & aggre_lev=="year") %>%
    select(dat_grp,esutype,reartype,defin_det_yr,s1,s1_var,N) %>% 
    mutate(S1_est=round(s1,3),
           S1_se=round(sqrt(s1_var),3)) %>%
    select(-s1,-s1_var) %>%
    tidyr::pivot_longer(cols = c(N,S1_est,S1_se))
  
  ann_estsTAB_rev1_det <- ann_estsTAB_all_spp_det %>% filter(esutype=="SR_Ch1") %>%
    tidyr::pivot_wider(names_from = c(reartype,name),values_from = c(value))
  
  ann_estsTAB_rev1_pool <- ann_estsTAB_all_spp_pooled %>% filter(esutype=="SR_Ch1") %>%
    tidyr::pivot_wider(names_from = c(reartype,name),values_from = c(value))

  out_ls <- list("det"=ann_estsTAB_rev1_det,
                 "pool"=ann_estsTAB_rev1_pool)
  
  if(write.cvs){
    write.csv(out_ls$"det",paste0(csv_base_nm,"det.csv"),row.names = F)
    write.csv(out_ls$"pool",paste0(csv_base_nm,"pool.csv"),row.names = F)
  }
  return(out_ls)
  }
  

# write.csv(ann_estsTAB_rev1_det,"ann_estsTAB_rev1_det.csv",row.names = F)
# write.csv(ann_estsTAB_rev1_pool,"ann_estsTAB_rev1_pool.csv",row.names = F)