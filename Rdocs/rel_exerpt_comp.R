
library(dplyr)

# unfiltered vs filtered release data
tagDF_rel_grps <- (readRDS("temp/tags_and_obs_comb_ls9825.rds"))$"tags_comb"  
head(tagDF_rel_grps)

obs_rel_grps2 <- readRDS("comp_files/obs_rel_grps_2_9825.rds")





# baseline does not include tags released at LGRRRR
rel_exerpts <- list()

unfilt_rel_tgs_tb <- tagDF_rel_grps %>% 
  filter(!lgr_rel) %>%
  group_by(esutype,reartype) %>%
  summarize(ntags=length(unique(tagid))) %>%
  mutate(esutype_reartype=paste0(esutype,"_",reartype))
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
