
get_CJS_count_ls <- function(dat_in,RT_COMB_in){
  
  stopifnot(is.logical(RT_COMB_in) & length(RT_COMB_in)!=0)
  
  out=list(
    "yr_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                            aggre_lev="year",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,DH_code) %>%
                              summarize(n_tags=length(unique(code))) %>%
                              pivot_wider(names_from=DH_code,values_from = n_tags) %>%
                              mutate(binID=as.numeric(defin_det_yr)-min(as.numeric(dat_in$defin_det_yr))+1,bin="year")),
    
    "mnth_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                              aggre_lev="month",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,month,DH_code) %>%
                                summarize(n_tags=length(unique(code))) %>% ungroup() %>% 
                                mutate(binID=month,bin="month") %>% select(-month) %>%
                                pivot_wider(names_from=DH_code,values_from = n_tags)), 
    
    "2week_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                               aggre_lev="2weeks",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,weeks2,DH_code) %>%
                                 summarize(n_tags=length(unique(code))) %>% ungroup() %>% 
                                 mutate(binID=weeks2,bin="weeks2") %>% select(-weeks2) %>%
                                 pivot_wider(names_from=DH_code,values_from = n_tags)), 
    
    "week_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                              aggre_lev="week",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,week1,DH_code) %>%
                                summarize(n_tags=length(unique(code))) %>% ungroup() %>% 
                                mutate(binID=week1,bin="week1") %>% select(-week1) %>%
                                pivot_wider(names_from=DH_code,values_from = n_tags)), 
    
    "3.5day_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                                aggre_lev="halfweek",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,days3.5,DH_code) %>%
                                  summarize(n_tags=length(unique(code))) %>% ungroup() %>% 
                                  mutate(binID=days3.5,bin="days3.5") %>% select(-days3.5) %>%
                                  pivot_wider(names_from=DH_code,values_from = n_tags)), 
    
    "day_grp_tab"=data.frame(RT_COMB=RT_COMB_in,
                             aggre_lev="day",dat_in %>% group_by(dat_grp,esutype,reartype,defin_det_yr,grp_code,day,DH_code) %>%
                               summarize(n_tags=length(unique(code))) %>% ungroup() %>% 
                               mutate(binID=day,bin="day") %>% select(-day) %>%
                               pivot_wider(names_from=DH_code,values_from = n_tags)))
  out
}
