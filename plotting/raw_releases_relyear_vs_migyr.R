library(dplyr)

headDF <- function(x){head(data.frame(x))}
# saveRDS(list("tags_comb_raw"=tags_comb_raw,"obs_comb_raw"=obs_comb_raw),
#         "temp/tags_and_obs_comb_raw_ls9825.rds")

tags_comb_raw <- (readRDS("temp/tags_and_obs_comb_raw_ls9825.rds"))$"tags_comb_raw"
obs_comb_raw <- (readRDS("temp/tags_and_obs_comb_raw_ls9825.rds"))$"obs_comb_raw"

table(obs_comb_raw$reartype)
# function for summarizing raw releases by esutype,rear type, and release year
# - includes transported fish 

source("C:/repos/CBR_juv_pit/R/get_raw_release_tab.R")
my_rel_tb <- get_raw_release_tab(tags_comb_raw_in = tags_comb_raw)

# table(is.na(tags_comb_raw$min_estMigry))
head(tags_comb_raw$min_estMigry)


######################################################################## #
# Looking at estimated migration year based on MCN or LGR detection
######################################################################## #

# tags_comb_raw %>% group_by(esutype,reartype,) %>% min_estMigry
# 165 not the same
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR==estMigyrMCN) %>% pull())
# 105 cases MCN later year than LGR
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR<estMigyrMCN) %>% pull())
# 55 cases LGR later year than MCN
table(tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR>estMigyrMCN) %>% pull())
# tags_comb_raw %>% filter(!is.na(estMigyrLGR) & !is.na(estMigyrMCN)) %>% mutate(same=estMigyrLGR==estMigyrMCN) %>% filter(!same)

# tags_comb_raw <- tags_comb_raw %>% mutate(min_estMigry=ifelse((!is.na(estMigyrLGR) | !is.na(estMigyrMCN)),min(c(estMigyrLGR,estMigyrMCN),na.rm=T),NA))
# tags_comb_raw <- tags_comb_raw %>% rename(min_estMigry=min_est_Migry)
head(tags_comb_raw)

table(is.na(tags_comb_raw$min_estMigry))


######################################################################################### #
# Looking at estimated release year versus migration year based on MCN or LGR detection
######################################################################################### #

library(ggplot2)
# long-form raw release data
my_rel_tb_l <- get_raw_release_tab(tags_comb_raw_in = tags_comb_raw,long=T)

# summary of migration years (includes transported fish)
subb_tb2 <- subb_tb1 %>% 
  mutate(yr_diff=paste0("y_",min_estMigry-rel_year)) %>%
  group_by(esutype,reartype,rel_year,yr_diff) %>%
  summarize(ntags=(length(unique(tagid)))) %>%
  tidyr::pivot_wider(values_from = ntags,names_from=yr_diff) %>% 
  left_join(my_rel_tb_l) %>% 
  relocate(esutype,reartype,rel_year,ntags) #%>% 

rel_det_delayDF <- data.frame(offset_relmigr_subb[,1:3],yr_plus0,yr_plus1,yr_plus2) %>% left_join(my_rel_tb_l) %>% relocate(esutype,reartype,rel_year,ntags)
rel_det_delayDF <- rel_det_delayDF %>% mutate(P_seen=(yr_plus0+yr_plus1+yr_plus2)/ntags,
                                              P_plus0=yr_plus0/(yr_plus0+yr_plus1+yr_plus2),
                                              P_plus1=yr_plus1/(yr_plus0+yr_plus1+yr_plus2),
                                              P_plus2=yr_plus2/(yr_plus0+yr_plus1+yr_plus2))

rel_det_delayDF_plt <- rel_det_delayDF %>%  select(esutype,reartype,rel_year,ntags,P_seen)
ggplot2::ggplot(data=rel_det_delayDF_plt,aes(y=P_seen,x=rel_year,color=reartype)) + facet_grid(reartype~esutype,scales="free_y") + 
  # geom_line() + 
  geom_point() + scale_x_continuous(limits=c(2002,2025))

# release year plot
ggplot2::ggplot(data=rel_det_delayDF_plt,aes(y=ntags,x=rel_year,color=reartype)) + facet_grid(esutype~reartype,scales="free_y") + 
  geom_bar(stat="identity") + scale_x_continuous(limits=c(2002,2025))

# matrix of proportions of tagged fish "seen" again in year of release (y=0) also y=1 and y=2
t(sapply(1:nrow(subb_tb2),function(ii) {
  c(subb_tb2$y_0[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T),
    subb_tb2$y_1[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T),
    subb_tb2$y_2[ii]/sum(c(subb_tb2$y_0[ii],subb_tb2$y_1[ii],subb_tb2$y_2[ii]),na.rm=T))
}))
