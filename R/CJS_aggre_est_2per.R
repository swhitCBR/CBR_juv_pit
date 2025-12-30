CJS_aggre_est_2per <- function(dat_in,DH_cols="DH_red"){
  
  aggre_dat_ls <- CJS_aggre_fxns(dat_in,DH_cols="DH_red")
  
  summ_tab1_tmp <- aggre_dat_ls$summ_tab1
  summ_tab2_tmp <- aggre_dat_ls$summ_tab2
  
  dat_ls_RTsep_tmp <- aggre_dat_ls$dat_ls_RTsep
  dat_ls_RTcomb_tmp <- aggre_dat_ls$dat_ls_RTcomb
  
  out_ls_RTsep_tmp <- lapply(dat_ls_RTsep_tmp,get_mom_mat)
  out_ls_RTcomb_tmp <- lapply(dat_ls_RTcomb_tmp,get_mom_mat)
  
  outDF <- rbind(
    do.call(rbind,out_ls_RTsep_tmp),
    do.call(rbind,out_ls_RTcomb_tmp))
  
  out <- list(
    "out_ls_RTsep"=out_ls_RTsep_tmp,
    "out_ls_RTcomb"=out_ls_RTcomb_tmp,
    "summ_tab1"=summ_tab1_tmp,
    "summ_tab2"=summ_tab2_tmp,
    "outDF"=outDF)
  
  return(out)
  
}

