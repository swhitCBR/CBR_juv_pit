CJS_aggre_fxns <- function(dat_in,DH_cols="DH_red"){
  
  require(dplyr)
  require(tidyr)
  
  stopifnot(length(DH_cols)==1 & any(DH_cols %in% names(dat_in)))
  dat_in$DH_code <- dat_in[,DH_cols]
  
  # tag-level table
  
  message("Aggregating datasets by: year, month(28 days), 2 weeks, week, 3.5 days, and day")
  
  ############################## #
  # Separate reartypes
  ############################## #
  
  # 140 scenarios
  summ_tab1 <- dat_in %>% 
    group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code) %>%
    summarize(days=length(unique(day)),
              days3.5=length(unique(days3.5)),
              week1 =length(unique(week1)),
              weeks2 =length(unique(weeks2)),
              month=length(unique(month)),
              years=length(unique(defin_det_yr))) %>%
    mutate(n_datasets=sum(days,days3.5,week1,weeks2,month,years))
  
  summ_tab2 <-  summ_tab1 %>% 
    group_by(dat_grp,esutype,reartype) %>%
    summarize(
      day_rng=paste(min(days),max_days=max(days),sep="-"),
      avg_days=mean(days),
      n_days=sum(days),
      n_weeks=sum(week1),
      n_months=sum(month),
      n_datasets=sum(n_datasets)) %>% arrange(dat_grp)
  
  ################################ #
  # Lists of data sets
  ################################ #
  
  message("Compiling datasets for separate rear types")
  dat_ls_RTsep <- get_CJS_count_ls(dat_in = dat_in,RT_COMB_in = FALSE)
  
  # message("Compiling datasets for combined rear types")
  # dat_in_RTCOMB <- dat_in %>% mutate(reartype="comb")
  # dat_in_RTCOMB <- dat_in_RTCOMB %>% mutate(grp_code=paste(dat_grp,esutype,"comb",defin_det_yr))
  # dat_ls_RTcomb <- get_CJS_count_ls(dat_in = dat_in_RTCOMB,RT_COMB_in = TRUE)
  
  out <- list(
    "summ_tab1"=summ_tab1,
    "summ_tab2"=summ_tab2,
    "dat_ls_RTsep"=dat_ls_RTsep#,
    # "dat_ls_RTcomb"=dat_ls_RTcomb
  )
  
  return(out)
  
}
