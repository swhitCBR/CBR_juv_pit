length(unique(obs_rel_grps2$tagid))


length(unique(obs_rel_grps3$tagid))/length(unique(obs_rel_grps2$tagid))

length(unique(obs_rel_grps4$tagid))/length(unique(obs_rel_grps2$tagid))

length(unique(obs_rel_grps5$tagid))/length(unique(obs_rel_grps2$tagid))

tg_drp_at5 <- unique(obs_rel_grps4$tagid[!obs_rel_grps4$tagid %in% obs_rel_grps5$tagid])

# length(tg_drp_at5)+length(unique(obs_rel_grps5$tagid))==length(unique(obs_rel_grps4$tagid))

# obs_rel 30% excluded tags, all virtual release fish
excluDF <- data.frame(obs_rel_grps4[obs_rel_grps4$tagid %in% tg_drp_at5,])
excluDF %>% group_by(esutype,event,detID_raw) %>% summarize(length(unique(tagid)))


obs_rel_grps2 %>% filter(tagid=="222F635313")


# excluDF %>% group_by(esutype,rel_year,event) %>% summarize(length(unique(tagid)))


obs_rel_grps5 %>% group_by(esutype,event) %>% summarize(length(unique(tagid)))


data.frame(obs_rel_grps2[obs_rel_grps4$tagid %in% tg_drp_at5[1],])

data.frame(obs_rel_grps4[obs_rel_grps4$tagid %in% tg_drp_at5[1],])

data.frame(obs_rel_grps4[obs_rel_grps4$tagid %in% tg_drp_at5[2],])



nrow(obs_rel_grps2)
