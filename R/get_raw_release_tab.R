
# summarizes counts of released fish (1996-2025)
# - does not exclude any transported fish

get_raw_release_tab <- function(tags_comb_raw_in,long=F){
  
  
  rel_tb_l <- tags_comb_raw_in %>% 
    filter(reartype!="U" & !rel_at_LGR) %>%  
    group_by(esutype,reartype,rel_year) %>% 
    summarize(ntags=length(unique(tagid))) #%>% 
  # mutate(reartype=factor(reartype,levels=c("W","H")))
  # %>%
  # mutate(code=paste0(esutype,"_",reartype))
  
  if(long) {return(rel_tb_l)}
  
  rel_tb_l %>% tidyr::pivot_wider(values_from = ntags,names_from=c(esutype,reartype)) %>%
    relocate(rel_year,SR_Ch1_W,SR_Ch1_H,SR_Sock_W,SR_Sock_H,SR_Sthd_W,SR_Sthd_H)

}

