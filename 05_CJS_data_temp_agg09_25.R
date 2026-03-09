# source("functions.R")

# code-level detection history with wide format bin names
mcn_dh_tab2 <- readRDS("comp_files/mcn_dh_tab2_9825_wPD568.rds")
mcn_dh_tab2$DH_red <- factor(paste(mcn_dh_tab2$BON,mcn_dh_tab2$Estuary),
                             levels=c("1 1","1 0","0 1","0 0"))

# bin attributes table
# bin_tab_ls_combDFwYR <- readRDS("temp/bin_tab_ls_combDFwYR.rds")

bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

source("R/get_CJS_count_ls.R")

source("R/CJS_aggre_fxns.R")
source("R/CJS_aggre_est_2per.R")
source("R/get_mom_mat.R")

MCN_cjs_dataset_ls <- CJS_aggre_fxns(dat_in=mcn_dh_tab2,DH_cols="DH_red")
lapply(MCN_cjs_dataset_ls$dat_ls_RTsep,dim)

# names(MCN_cjs_dataset_ls)
# lapply(MCN_cjs_dataset_ls$dat_ls_RTcomb,dim)
which(is.na(MCN_cjs_dataset_ls$dat_ls_RTsep$day_grp_tab$binID))

table(is.na(mcn_dh_tab2$day))
mcn_dh_tab2[is.na(mcn_dh_tab2$day),]

MCN_cjs_dataset_ls$dat_ls_RTsep$day_grp_tab[43,]
MCN_cjs_dataset_ls$dat_ls_RTsep$day_grp_tab[MCN_cjs_dataset_ls$dat_ls_RTsep$day_grp_tab$grp_code=="mcn_det SR_Ch1 H 2009",]

MCN_all_out <- CJS_aggre_est_2per(dat_in=mcn_dh_tab2,DH_cols="DH_red")


# all_out$summ_tab1
# all_out$summ_tab2

mcn_est_outDF <- MCN_all_out$outDF
head(mcn_est_outDF)
table(is.na(mcn_est_outDF$binID))

mcn_est_outDF2 <- mcn_est_outDF %>%
  mutate(bin_code=paste(grp_code,bin,binID)) %>% 
  left_join(bin_tab_ls_combDFwYR %>% 
              mutate(bin_code=paste(grp_code,bin,binID)))

mcn_RT_rep_tab <- mcn_est_outDF2 %>% filter(!RT_COMB) %>%
  group_by(dat_grp,esutype,defin_det_yr,aggre_lev,bin,binID) %>% 
  summarize(RT_rep=paste(unique(reartype),collapse = ",")) %>%
  mutate(grp_code=paste(dat_grp,esutype,"comb",defin_det_yr), 
         code3=paste(grp_code,bin,binID)) 

mcn_est_outDF3 <- mcn_est_outDF2 %>% 
  mutate(bin_cov_prop=ind_bin_hours/full_bin_hours,
         code3=paste(grp_code,bin,binID)) %>% 
  left_join(mcn_RT_rep_tab) %>%
  mutate(RT_rep=ifelse(reartype!="comb",reartype,RT_rep))
# View(mcn_est_outDF3)

dir.create("est_files")
saveRDS(mcn_est_outDF3,"est_files/mcn_est_outDF3.rds")



table(mcn_est_outDF$reartype)


gc()
# code-level detection history with wide format bin names
lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
lgr_dh_tab2$DH_red <- factor(paste(lgr_dh_tab2$BON,lgr_dh_tab2$Estuary),
                             levels=c("1 1","1 0","0 1","0 0"))

# bin attributes table
# bin_tab_ls_combDFwYR <- readRDS("temp/bin_tab_ls_combDFwYR.rds")
bin_tab_ls_combDFwYR <- readRDS("comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")


LGR_cjs_dataset_ls <- CJS_aggre_fxns(dat_in=lgr_dh_tab2,DH_cols="DH_red")
lapply(LGR_cjs_dataset_ls$dat_ls_RTsep,dim)
lapply(LGR_cjs_dataset_ls$dat_ls_RTcomb,dim)


LGR_all_out <- CJS_aggre_est_2per(dat_in=lgr_dh_tab2,DH_cols="DH_red")
lgr_est_outDF <- LGR_all_out$outDF
head(lgr_est_outDF)

lgr_est_outDF2 <- lgr_est_outDF %>%
  mutate(bin_code=paste(grp_code,bin,binID)) %>% 
  left_join(bin_tab_ls_combDFwYR %>% 
              mutate(bin_code=paste(grp_code,bin,binID)))

# what are wee looking at here? 
lgr_est_outDF2 %>% filter(RT_COMB)


saveRDS(lgr_est_outDF2,"est_files/lgr_est_outDF2.rds")
saveRDS(mcn_est_outDF2,"est_files/mcn_est_outDF2.rds")


######################################## #
# 
######################################## #

lgr_RT_rep_tab <- lgr_est_outDF2 %>% filter(!RT_COMB) %>%
  group_by(dat_grp,esutype,defin_det_yr,aggre_lev,bin,binID) %>% 
  summarize(RT_rep=paste(unique(reartype),collapse = ",")) %>%
  mutate(grp_code=paste(dat_grp,esutype,"comb",defin_det_yr), 
         code3=paste(grp_code,bin,binID)) 

lgr_est_outDF3 <- lgr_est_outDF2 %>% 
  mutate(bin_cov_prop=ind_bin_hours/full_bin_hours,
         code3=paste(grp_code,bin,binID)) %>% 
  left_join(lgr_RT_rep_tab) %>%
  mutate(RT_rep=ifelse(reartype!="comb",reartype,RT_rep))

saveRDS(lgr_est_outDF3,"temp/lgr_est_outDF3.rds")
saveRDS(mcn_est_outDF3,"est_files/mcn_est_outDF3.rds")


source("R/get_ests_from_MSS.R")


lgr_est_outDF4 <- data.frame(lgr_est_outDF3,t(sapply(1:nrow(lgr_est_outDF3),function(x) per2_surph_ests(cell_vals_in = unlist(lgr_est_outDF3[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))
mcn_est_outDF4 <- data.frame(mcn_est_outDF3,t(sapply(1:nrow(mcn_est_outDF3),function(x) per2_surph_ests(cell_vals_in = unlist(mcn_est_outDF3[x,c("n.11","n.10","n.01","n.00")]),w_table=F))))


# MCN_BON reach
MCN_BON_per_2_mods_outDF_sep <- bind_rows(
  # MCN to BON separate rear types
  data.frame(grp="MCN_BON_wMCNtags",mcn_est_outDF4 %>%  filter(!RT_COMB)),
  # LGR fish MCN-BON  2-period model
  data.frame(grp="MCN_BON_wLGRtags",lgr_est_outDF4) %>%  filter(!RT_COMB))

