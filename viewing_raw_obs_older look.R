library(dplyr)
library(ggplot2)

tags_and_obs_comb_raw_ls <- readRDS("temp/tags_and_obs_comb_raw_ls9825.rds")
tags_comb_raw <- tags_and_obs_comb_raw_ls$"tags_comb_raw"
obs_comb_raw <- tags_and_obs_comb_raw_ls$"obs_comb_raw"

tags_comb_raw$rel_year <- lubridate::year(tags_comb_raw$reltime)
tags_comb_raw$at_LGR <- tags_comb_raw$relsite=="LGRRRR"
# obs_comb_raw
obs_comb_raw %>% filter(esutype=="SR_Ch1" & defin_det_yr==2013)


obs_rel_grps2 <- readRDS("comp_files/obs_rel_grps_2_9825.rds")
obs_rel_grps3 <- readRDS("comp_files/obs_rel_grps_3_9825.rds")

obs_rel_grps2 %>% 
  filter(esutype=="SR_Ch1" & 
           year==2013 & #
           prim_loc_cat =="LGR" & 
           lgr_det) %>% 
  group_by(dat_grp,reartype) %>% 
  summarize(ntags=length(unique(tagid)))

table(obs_rel_grps2$dat_grp)


tmp_tb <- obs_rel_grps2 %>% 
  filter(dat_grp=="lgr_pooled" &
           esutype=="SR_Ch1" & 
           year==2013 & relsite=="LGRRRR")

# all of these are false
table(tmp_tb$lgr_det)
table(tmp_tb$obssite_prim)
table(tmp_tb$obssiteORIG)
table(tmp_tb$prim_loc_cat)
table(tmp_tb$obssite)



obs_rel_grps3 %>% 
  filter(esutype=="SR_Ch1" & 
           year==2013 & #
           prim_loc_cat =="LGR" & 
           lgr_det) %>% 
  group_by(reartype) %>% 
  summarize(ntags=length(unique(tagid)))

table(obs_rel_grps2$unassagn)
table(obs_rel_grps2$defin_det_yr,obs_rel_grps2$unassagn)

table(is.na(obs_rel_grps3$unassagn))




stlhd_tags_raw<- tags_comb_raw %>%
  filter(esutype=="SR_Sthd") %>%
  group_by(rel_year,reartype,trans_statTF,at_LGR,relsite) %>% 
  summarise(n_tags=length(unique(tagid)),
            n_rel_sites=length(unique(relsite)))

ggplot(data=stlhd_tags_raw,aes(x=rel_year,n_tags,fill=at_LGR)) + 
  geom_bar(stat="sum") + facet_grid(trans_statTF~reartype) 

head(stlhd_tags_raw)

stlhd_tags_raw %>% 
  filter(relsite!="LGRRRR") %>%
  group_by(rel_year) %>% 
  summarize(sum(n_tags),length(n_tags ))




# Steve Smith has more fish tagged at LGRRRR
ggplot(data=stlhd_tags_raw %>% filter(at_LGR,rel_year>1998),
       aes(x=rel_year,n_tags,fill=trans_statTF)) + 
  geom_bar(stat="sum") + facet_grid(~reartype) +
  ggtitle("Wild SR Steelhead tagged at LGR")


ggplot(data=stlhd_tags_raw %>% filter(at_LGR,rel_year>1998),
       aes(x=rel_year,n_tags,fill=trans_statTF)) + 
  geom_bar(stat="sum") + facet_grid(~reartype) +
  ggtitle("Wild SR Steelhead tagged at LGR")




tags_and_obs_comb_ls <- readRDS("temp/tags_and_obs_comb_ls9825.rds")
tags_comb <- tags_and_obs_comb_ls$"tags_comb"
obs_comb <- tags_and_obs_comb_ls$"obs_comb"

message(signif(x=nrow(tags_comb),digits = 4)/1000000," Million tags")

# tags_comb$species
head(tags_comb)
tags_comb$rel_year <- lubridate::year(tags_comb$reltime)
tags_comb$at_LGR <- tags_comb$relsite=="LGRRRR"

stlhd_tags<- tags_comb %>%
  filter(esutype=="SR_Sthd") %>%
  group_by(rel_year,reartype,at_LGR,relsite) %>% 
  summarise(n_tags=length(unique(tagid)),
            n_rel_sites=length(unique(relsite)))


ggplot(data=stlhd_tags,aes(x=rel_year,n_tags,fill=relsite)) + 
  geom_bar(stat="sum") + 
  theme(legend.position = "none")

ggplot(data=stlhd_tags,aes(x=rel_year,n_tags,fill=at_LGR)) + 
  geom_bar(stat="sum") + facet_wrap(~reartype) 


ggplot(data=stlhd_tags,aes(x=rel_year,n_tags,fill=relsite)) + 
  geom_bar(stat="sum") + 
  theme(legend.position = "none")

ggplot(data=stlhd_tags,aes(x=rel_year,n_tags,fill=relsite)) + 
  geom_bar(stat="sum") + 
  theme(legend.position = "none")

# Steve Smith has more fish tagged at LGRRRR
ggplot(data=stlhd_tags_raw %>% filter(at_LGR,rel_year>2010),
       aes(x=rel_year,n_tags,fill=trans_statTF)) + 
  geom_bar(stat="sum") + facet_grid(~reartype) +
  ggtitle("Wild SR Steelhead tagged at LGR after removing some fish")


