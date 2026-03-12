
library(dplyr)

# unfiltered vs filtered release data
# tagDF_rel_grps_raw <- (readRDS("comp_files/tags_and_obs_comb_ls9825.rds"))$"tags_comb"
# tagDF_rel_grps_raw <- (readRDS("comp_files/DART_ALL_SR_ESU_DPS_rec_ls.rds"))$"tagDF"

tagDF_rel_grps_raw <- readRDS("temp/tags_and_obs_comb_raw_ls9825.rds")$"tags_comb_raw"

str(tagDF_rel_grps_raw)
names(tagDF_rel_grps_raw)

# str(tagDF_rel_grps)
wLGRRRR_rel_tgs_esu_tb_raw <- tagDF_rel_grps_raw %>% 
  group_by(esutype,rel_year) %>% 
  summarize(ntags=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=esutype,values_from = ntags) %>%
  arrange(rel_year)

wLGRRRR_rel_tgs_esuexclu_tb_raw <- tagDF_rel_grps_raw %>% 
  group_by(excluded,esutype,rel_year) %>% 
  summarize(ntags=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=c(excluded,esutype),values_from = ntags) %>%
  arrange(rel_year)

wLGRRRR_rel_tgs_esurt_tb_raw <- tagDF_rel_grps_raw %>% 
  mutate(esutype_reartype=paste0(esutype,"_",reartype)) %>%
  group_by(esutype_reartype,rel_year) %>% 
  summarize(ntags=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=esutype_reartype,values_from = ntags) %>%
  arrange(rel_year)

wLGRRRR_rel_tgs_esu_tb_raw
wLGRRRR_rel_tgs_esurt_tb_raw

# lowest and highest for sockeye
# barplots of release sizes



# tags inclded in final 'obs_rel_grps10' grouping shown as T/F in column 'tagid_inclu'
tagDF_rel_grps <- readRDS("comp_files/tags_comb_inclu_9825.rds")
str(tagDF_rel_grps)


#########################################################
# baseline does not include tags released at LGRRRR
#########################################################

# tables 
base_rel_tgs_tb <- tagDF_rel_grps %>% 
  filter(!lgr_rel) %>%
  group_by(esutype,reartype) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype)) %>%
  mutate(grouping="base")

base_rel_tgs_rtcomb_tb <- base_rel_tgs_tb %>% group_by(grouping,esutype) %>% summarize(ntags=sum(ntags))

base_rel_tgs_HR_prop_tb <- base_rel_tgs_tb %>% 
  select(grouping,esutype,reartype,ntags) %>% 
  tidyr::pivot_wider(values_from=ntags,names_from=reartype) %>% 
  mutate(tot=W+H,W_prop=W/tot,H_prop=H/tot)


wLGRRRR_rel_tgs_tb <- tagDF_rel_grps %>% 
  group_by(esutype,reartype) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype)) %>%
  mutate(grouping="wLGRRRR")

wLGRRRR_rel_tgs_rtcomb_tb <- wLGRRRR_rel_tgs_tb %>% group_by(grouping,esutype) %>% summarize(ntags=sum(ntags))

wLGRRRR_rel_tgs_HR_prop_tb <- wLGRRRR_rel_tgs_tb %>% 
  select(grouping,esutype,reartype,ntags) %>% 
  tidyr::pivot_wider(values_from=ntags,names_from=reartype) %>% 
  mutate(tot=W+H,W_prop=W/tot,H_prop=H/tot) 

wLGRRRR_tg_cnt_tb <- base_rel_tgs_tb %>% 
  bind_rows(wLGRRRR_rel_tgs_tb) %>% 
  # filter(reartype=="W") %>% 
  tidyr::pivot_wider(values_from = ntags ,names_from = grouping,names_prefix = "ntags_")  %>%
  mutate(LGRRRR_diff=ntags_wLGRRRR-ntags_base)  %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype)) 

wLGRRRR_tg_cnt_v <- wLGRRRR_tg_cnt_tb %>% pull(LGRRRR_diff)
names(wLGRRRR_tg_cnt_v) <- wLGRRRR_tg_cnt_tb %>% pull(esutype_reartype)


wLGRRRR_diff_wprop_tb <- base_rel_tgs_HR_prop_tb %>% 
  select(grouping,esutype,W_prop) %>%  
  bind_rows(wLGRRRR_rel_tgs_HR_prop_tb %>% 
              select(grouping,esutype,W_prop)) %>%
  tidyr::pivot_wider(values_from = W_prop,names_from = grouping,names_prefix = "W_prop_") %>%
  mutate(W_prop_times_gr=W_prop_wLGRRRR/W_prop_base,
         W_prop_diff=W_prop_wLGRRRR-W_prop_base) 

# multiplicative difference in propotion
# wLGRRRR_diff_wprop_v <- wLGRRRR_diff_wprop_tb %>% pull(W_prop_times_gr)
# names(wLGRRRR_diff_wprop_v) <- wLGRRRR_diff_wprop_tb %>% pull(esutype)


W_prop_wLGRRRR_v <- wLGRRRR_diff_wprop_tb %>% pull(W_prop_wLGRRRR)
names(W_prop_wLGRRRR_v) <- wLGRRRR_diff_wprop_tb %>% pull(esutype)





# named vectors 
base_rel_tgs_v <- base_rel_tgs_tb %>% pull(ntags)
names(base_rel_tgs_v) <- base_rel_tgs_tb %>% pull(esutype_reartype)

base_rel_tgs_rtcomb_v <- base_rel_tgs_rtcomb_tb %>% pull(ntags)
names(base_rel_tgs_rtcomb_v) <- base_rel_tgs_rtcomb_tb %>% pull(esutype)

base_rel_tgs_W_prop_v <- base_rel_tgs_HR_prop_tb %>% pull(W_prop)
names(base_rel_tgs_W_prop_v) <- base_rel_tgs_HR_prop_tb %>% pull(esutype)

# check validity
all.equal(
  sum(base_rel_tgs_tb$ntags),
  sum(base_rel_tgs_rtcomb_tb$ntags),
  sum(base_rel_tgs_v),
  sum(base_rel_tgs_rtcomb_v))


rel_exerpts <- list()

# filling in 
rel_exerpts$"tot_rel_all" <- sum(base_rel_tgs_v)
rel_exerpts$"base_rel_tgs_v" <- base_rel_tgs_v
rel_exerpts$"base_rel_tgs_rtcomb_v" <- base_rel_tgs_rtcomb_v
rel_exerpts$"base_rel_tgs_W_prop_v" <- base_rel_tgs_W_prop_v
rel_exerpts$"wLGRRRR_tg_cnt_v" <- wLGRRRR_tg_cnt_v
# rel_exerpts$"wLGRRRR_diff_wprop_v" <- wLGRRRR_diff_wprop_v
rel_exerpts$"W_prop_wLGRRRR_v" <- W_prop_wLGRRRR_v




# wLGRRRR_tg_cnt_v

lgr_rel_tgs_rel_year_tb <- tagDF_rel_grps %>% 
  # filter(lgr_rel & reartype=="W" & esutype!="SR_Sock") %>%
  group_by(esutype,rel_year) %>%
  summarize(ntags=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=esutype,values_from = ntags) %>%
  arrange(rel_year)







lgr_rel_tgs_rel_year_tb <- tagDF_rel_grps %>% 
  filter(lgr_rel & reartype=="W" & esutype!="SR_Sock") %>%
  group_by(esutype,rel_year) %>%
  summarize(ntags=length(unique(tagid))) %>%
  tidyr::pivot_wider(names_from=esutype,values_from = ntags) %>%
  arrange(rel_year)

lgr_rel_tgs_tb <- tagDF_rel_grps %>% 
  filter(lgr_rel & reartype=="W" & esutype!="SR_Sock") %>%
  group_by(esutype,reartype) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype))

lgr_rel_tgs_inclu_tb <- tagDF_rel_grps %>% 
  filter(lgr_rel & reartype=="W" & esutype!="SR_Sock") %>%
  group_by(esutype,reartype,tagid_inclu) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype))


unfilt_rel_tgs_inclu_tb_lgrrel <- tagDF_rel_grps %>% 
  # filter(!lgr_rel) %>%
  group_by(esutype,reartype,lgr_rel,tagid_inclu) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype)) %>%
  tidyr::pivot_wider(names_from=tagid_inclu,values_from = ntags)

# unfilt_rel_tgs_inclu_tb_lgrrel <- tagDF_rel_grps %>% 
#   # filter(!lgr_rel) %>%
#   group_by(esutype,reartype,lgr_rel,tagid_inclu) %>%
#   summarize(ntags=length(unique(tagid))) %>%
#   mutate(esutype_reartype=paste0(esutype,"_",reartype)) %>% 
#   tidyr::pivot_wider(names_from=tagid_inclu,values_from = ntags)




# rel_exerpts$"unfilt_rel_tgs_v" <- unfilt_rel_tgs_v
# rel_exerpts$"unfilt_rel_tgs_rtcomb_v" <- unfilt_rel_tgs_v


# prettyNum


# rel_exerpts$tot_rel_all <- prettyNum(nrow(tagDF_rel_grps) , big.mark = ",", scientific = FALSE)
# rel_exerpts$tot_rel_ <- prettyNum(nrow(tagDF_rel_grps) , big.mark = ",", scientific = FALSE)
# rel_exerpts$tot_rel_all <- prettyNum(nrow(tagDF_rel_grps) , big.mark = ",", scientific = FALSE)
# rel_exerpts$tot_rel_all <- prettyNum(nrow(tagDF_rel_grps) , big.mark = ",", scientific = FALSE)


# prettyNum(x, big.mark = ",", scientific = FALSE)

saveRDS(rel_exerpts,"Rdocs/tag_rel_exerpts_ls.rds")

# readRDS("Rdocs/tag_rel_exerpts_ls.rds")



# filtered data
# tagDF_rel_9 <- readRDS("temp/tagDF_rel_9.rds")
