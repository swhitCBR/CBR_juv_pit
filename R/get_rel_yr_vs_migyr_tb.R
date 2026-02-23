
get_rel_yr_vs_migyr_tb <- function(tags_comb_raw_in){

  Ch1_W_relyr_migyr <- tags_comb_raw_in %>% 
      filter(esutype=="SR_Ch1" & reartype=="W" & LGR_loc_det) %>%  
      group_by(esutype,reartype,LGR_loc_det,rel_year,estMigyr) %>% 
      summarize(ntags=length(unique(tagid))) %>% 
      mutate(lb="LGRd") %>%
      tidyr::pivot_wider(values_from = ntags,names_from=c(estMigyr))
  
  Ch1_H_relyr_migyr <- tags_comb_raw_in %>% 
    filter(esutype=="SR_Ch1" & reartype=="H" & LGR_loc_det) %>%  
    group_by(esutype,reartype,LGR_loc_det,rel_year,estMigyr) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="LGRd") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(estMigyr))
  
  
  Sthd_W_relyr_migyr <- tags_comb_raw_in %>% 
    filter(esutype=="SR_Sthd" & reartype=="W" & LGR_loc_det) %>%  
    group_by(esutype,reartype,LGR_loc_det,rel_year,estMigyr) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="LGRd") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(estMigyr))
  
  Sthd_H_relyr_migyr <- tags_comb_raw_in %>% 
    filter(esutype=="SR_Sthd" & reartype=="H" & LGR_loc_det) %>%  
    group_by(esutype,reartype,LGR_loc_det,rel_year,estMigyr) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="LGRd") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(estMigyr))
  
  }