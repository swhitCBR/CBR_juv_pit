
library(dplyr)

# unfiltered vs filtered release data
# tagDF_rel_grps <- (readRDS("comp_files/tags_and_obs_comb_ls9825.rds"))$"tags_comb"
# str(tagDF_rel_grps)

# tags inclded in final 'obs_rel_grps10' grouping shown as T/F in column 'tagid_inclu'
tagDF_rel_grps <- readRDS("comp_files/tags_comb_inclu_9825.rds")
str(tagDF_rel_grps)


# obs_rel_grps2 <- readRDS("comp_files/obs_rel_grps_2_9825.rds")
# obs_rel_grps9 <- readRDS("comp_files/obs_rel_grps_9_9825.rds")
# obs_rel_grps10 <- readRDS("comp_files/obs_rel_grps_10_9825.rds")


# baseline does not include tags released at LGRRRR
rel_exerpts <- list()

unfilt_rel_tgs_tb <- tagDF_rel_grps %>% 
  filter(!lgr_rel) %>%
  group_by(esutype,reartype) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype))


# unfilt_rel_tgs_tb <- tagDF_rel_grps %>% 
#   filter(!lgr_rel) %>%
#   group_by(rel_year,esutype,reartype) %>%
#   summarize(ntags=length(unique(tagid))) %>%
#   mutate(esutype_reartype=paste0(esutype,"_",reartype))



unfilt_rel_tgs_v <- unfilt_rel_tgs_tb %>% pull(ntags)
names(unfilt_rel_tgs_v) <- unfilt_rel_tgs_tb %>% pull(esutype_reartype)

unfilt_rel_tgs_rtcomb_tb <- unfilt_rel_tgs_tb %>% group_by(esutype) %>% summarize(ntags=sum(ntags))
unfilt_rel_tgs_rtcomb_v <- unfilt_rel_tgs_rtcomb_tb %>% pull(ntags)
names(unfilt_rel_tgs_rtcomb_v) <- unfilt_rel_tgs_rtcomb_tb %>% pull(esutype)

unfilt_rel_tgs_HR_prop_tb <- unfilt_rel_tgs_tb %>% 
  select(esutype,reartype,ntags) %>% 
  tidyr::pivot_wider(values_from=ntags,names_from=reartype) %>% 
  mutate(tot=W+H,W_prop=W/tot,H_prop=H/tot) 
unfilt_rel_tgs_W_prop_v <- unfilt_rel_tgs_HR_prop_tb %>% pull(W_prop)
names(unfilt_rel_tgs_W_prop_v) <- unfilt_rel_tgs_HR_prop_tb %>% pull(esutype)

############## #
# confirming
############## #

all.equal(
  sum(unfilt_rel_tgs_tb$ntags),
  sum(unfilt_rel_tgs_rtcomb_tb$ntags),
  sum(unfilt_rel_tgs_v),
  sum(unfilt_rel_tgs_rtcomb_v))



lgr_rel_tgs_rel_year_tb <- tagDF_rel_grps %>% 
  filter(lgr_rel & reartype=="W" & esutype!="SR_Sock") %>%
  # select(-reartype) %>%
  group_by(esutype,rel_year) %>%
  summarize(ntags=length(unique(tagid))) %>%
  # mutate(esutype_reartype=paste0(esutype,"_",reartype))   %>%
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



rel_exerpts$"tot_rel_all" <- sum(unfilt_rel_tgs_v)
rel_exerpts$"unfilt_rel_tgs_v" <- unfilt_rel_tgs_v
rel_exerpts$"unfilt_rel_tgs_rtcomb_v" <- unfilt_rel_tgs_rtcomb_v
rel_exerpts$"unfilt_rel_tgs_W_prop_v" <- unfilt_rel_tgs_W_prop_v


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
tagDF_rel_9 <- readRDS("temp/tagDF_rel_9.rds")
