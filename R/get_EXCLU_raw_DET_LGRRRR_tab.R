
get_EXCLU_raw_DET_LGRRRR_tab <- function(tags_comb_raw_in){
  
  LGR_rel_tb_w <- tags_comb_raw_in %>% 
    filter(reartype!="U" & rel_at_LGR & reartype!="H" & esutype!="SR_Sock" & excluded) %>%  
    group_by(esutype,reartype,rel_at_LGR,rel_year) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="LGRr") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,lb))
  
  LGR_det_tb_w <- tags_comb_raw_in %>% 
    filter(reartype!="U" & LGR_loc_det & excluded) %>%  
    group_by(esutype,reartype,LGR_loc_det,rel_year) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="LGRd") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,lb))
  
  MCN_det_tb_w <- tags_comb_raw_in %>% 
    filter(reartype!="U" & MCN_loc_det & excluded) %>%  
    group_by(esutype,reartype,MCN_loc_det,rel_year) %>% 
    summarize(ntags=length(unique(tagid))) %>% 
    mutate(lb="MCNd") %>%
    tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,lb))
  
  # bind_cols(LGR_rel_tb_w,MCN_det_tb_w,MCN_det_tb_w)
  list(LGR_rel_tb_w,MCN_det_tb_w,MCN_det_tb_w)
  
  # det_LGR_rel_tb_l <- tags_comb_raw_in %>% 
  #   filter(reartype!="U") %>%  
  #   group_by(esutype,reartype,rel_at_LGR,LGR_loc_det,MCN_loc_det,rel_year) %>% 
  #   summarize(ntags=length(unique(tagid))) #%>% 
  
  
  # mutate(reartype=factor(reartype,levels=c("W","H")))
  # %>%
  # mutate(code=paste0(esutype,"_",reartype))
  # return(det_LGR_rel_tb_l)
  
  # det_LGR_rel_tb_l %>% tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype,rel_at_LGR)) #%>%
  #   relocate(rel_year,SR_Ch1_W,SR_Ch1_H,SR_Sock_W,SR_Sock_H,SR_Sthd_W,SR_Sthd_H)
  
}