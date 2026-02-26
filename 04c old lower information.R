
dh_tab <- obs_rel_grps9 %>% 
  group_by(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,grp_code,code,tagid) %>%
  summarize(DH_label=paste(prim_loc_cat,collapse=" -> ")) %>%
  mutate(avian_rec=tagid %in% c(rel_recov_tgs))

gc()
# <- tagid 
################################## #
# adding avian recoveries
################################## #

dh_subb <- dh_tab[dh_tab$tagid %in% rel_recov_tgs,]

unique(substr(x=dh_tab$DH_label,(nchar(dh_tab$DH_label)-9),nchar(dh_tab$DH_label)))
table(substr(x=dh_tab$DH_label,(nchar(dh_tab$DH_label)-9),nchar(dh_tab$DH_label))=="-> Estuary")

# avian_rec <- tagid %in% c(rel_recov_tgs)
dh_tab$avian_rec <- dh_tab$tagid %in% c(rel_recov_tgs)

dh_tab$no_estu_end=substr(x=dh_tab$DH_label,(nchar(dh_tab$DH_label)-9),nchar(dh_tab$DH_label))!="-> Estuary"
dh_tab$av_recov=dh_tab$no_estu_end & dh_tab$avian_rec
dh_tab$DH_label_mod=ifelse(dh_tab$av_recov," -> Estuary",dh_tab$DH_label)

# dh_tab<- dh_tab %>% mutate(
#   no_estu_end=substr(x=DH_label,(nchar(DH_label)-9),nchar(DH_label))!="-> Estuary",
#   av_recov=dh_tab$no_estu_end & dh_tab$avian_rec)#,#,paste0(dh_tab$DH_label," -> Estuary"),dh_tab$DH_label)
# dh_tab$DH_label_mod=ifelse(dh_tab$av_recov," -> Estuary",dh_tab$DH_label)
# 
head(dh_tab)

dh_tab %>% ungroup() %>% select(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,DH_label_mod) %>% filter(dat_grp=="lgr_det")

# dh_tab %>% ungroup() %>% select(dat_grp,esutype,reartype,defin_det_yr,defin_det_time_grp,DH_label,DH_label_mod) %>% filter(dat_grp=="lgr_det")
# head(dh_tab)
# View(dh_tab[dh_tab$DH_label_mod!=dh_tab$DH_label,])
# look for transition patterns that should be omitted because they are probably adults 
# manually replace as neccessary
table(dh_tab$DH_label) 

# pre-loaded tables describing detection histories for 2 locations
# head(LGR_DH_matchDF)
# head(MCN_DH_matchDF)

source("R/LGR_DH_match & MCN_DH_match.R")

# dh_tab_2

dh_tab <- dh_tab %>% mutate(DH_label_orig=DH_label,DH_label=DH_label_mod)

str(dh_tab)

bt=proc.time()
lgr_dh_tab <- dh_tab %>% filter(dat_grp %in% c("lgr_det","lgr_pooled")) %>% left_join(LGR_DH_matchDF)
proc.time()-bt # takes ~ 6.5 mins

bt=proc.time()
mcn_dh_tab <- dh_tab %>% filter(dat_grp=="mcn_det") %>% left_join(MCN_DH_matchDF)
proc.time()-bt # takes ~ 6.5 mins


lgr_dh_tab
mcn_dh_tab
# bt=proc.time()
# lgr_dh_tab <- dh_tab %>% filter(dat_grp %in% c("lgr_det","lgr_pooled")) %>% left_join(LGR_DH_matchDF)
# mcn_dh_tab <- dh_tab %>% filter(dat_grp=="mcn_det") %>% left_join(MCN_DH_matchDF)
# proc.time()-bt # takes ~ 6.5 mins

head(data.frame(lgr_dh_tab))
lgr_dh_tab %>% filter(esutype=="SR_Ch1" & reartype=W) %>% group_by(rel_year,)


################################################### #
# nas removed because dh_tab and obs_rel_grps9 have all the rows and 
# obs_rel_grps8 is filtered to only within init_det
# but obs_rel_grps8 has temporal bin assignments


# identifying codes(grp_code + tagid) in obs_rel_grps9 that 
mtc1 <- match(lgr_dh_tab$code,obs_rel_grps8$code)
mtc2 <- match(mcn_dh_tab$code,obs_rel_grps8$code)
table(is.na(mtc1)) # there are some nas
table(is.na(mtc2))


# why are there thousands of unmatched definitive detections for obs_rel8?
table(!lgr_dh_tab$code %in% obs_rel_grps8$code)
lgr_dh_tab[!lgr_dh_tab$code %in% obs_rel_grps8$code,]




head(obs_rel_grps8)
bt=proc.time()

lgr_dh_tab2 <- data.frame(lgr_dh_tab[!is.na(mtc1),],obs_rel_grps8[mtc1[!is.na(mtc1)],c("relsite","day","days3.5","week1","weeks2","month")])
mcn_dh_tab2 <- data.frame(mcn_dh_tab[!is.na(mtc2),],obs_rel_grps8[mtc2[!is.na(mtc2)],c("relsite","day","days3.5","week1","weeks2","month")])

lgr_dh_tab2$virt_det <- lgr_dh_tab2$relsite=="LGRRRR"
mcn_dh_tab2$virt_det <- mcn_dh_tab2$relsite=="LGRRRR"

proc.time()-bt # takes ~ 6.5 mins

# lgr_dh_tab2
# mcn_dh_tab2
gc()

nrow(bin_tab_ls_combDF)
nrow(bin_tab_ls_combDF %>% filter(bin=="day" & binID %in% 1:(length(unique(bin_tab_ls_combDF$defin_det_yr)))))
# 15*

yr_binsDF <- bin_tab_ls_combDF %>% 
  group_by(dat_grp,esutype,defin_det_yr,reartype,grp_code,official_strt,official_end) %>%
  summarize(bins_in_grp=length(binID),
            subyear_bin_types=length(unique(bin))) %>%
  mutate(binID=as.numeric(defin_det_yr)-min(as.numeric(bin_tab_ls_combDF$defin_det_yr))+1,
         bin="year",
         bin_strt=official_strt,
         bin_end=official_end,
         partial=FALSE,
         ind_bin_hours=as.numeric(difftime(official_end,official_strt,units = "hours")),
         full_bin_hours=ind_bin_hours,
         ind_bin_days=ind_bin_hours/24
  )

bin_tab_ls_combDFwYR <- bind_rows(bin_tab_ls_combDF,yr_binsDF) %>% 
  select(-bins_in_grp,-subyear_bin_types) %>% 
  select(-binID.1,-bin.1,-grp_code.1)

# last 267 rows are years
bin_tab_ls_combDFwYR$unq_binID <- 1:nrow(bin_tab_ls_combDFwYR)


mcn_dh_tab2

# 
# saveRDS(obs_rel_grps9,"comp_files/obs_rel_grps9_9825_wPD568.rds")
# saveRDS(obs_rel_grps8,"comp_files/obs_rel_grps8_9825_wPD568.rds")
# 
# 
# 
# saveRDS(lgr_dh_tab2,"comp_files/lgr_dh_tab2_9825_wPD568.rds")
# saveRDS(mcn_dh_tab2,"comp_files/mcn_dh_tab2_9825_wPD568.rds")
# # saveRDS(DF,"temp/diff_time_frst_to_99p_9823_wPD568.rds")
# saveRDS(bin_tab_ls_combDF,"temp/bin_tab_ls_combDF.rds")

saveRDS(bin_tab_ls_combDFwYR,"comp_files/bin_tab_ls_combDFwYR_9825_wPD568.rds")

# save.image("comp_files/04a_comp_env_20gb.Rdata")
