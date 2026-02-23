
get_raw_tag_brkdwn <- function(tags_comb_raw_in){
  summ_tb_tmp1 <- tags_comb_raw_in %>% 
    group_by(esutype,reartype,rel_year,event,rel_at_LGR,trans_stat) %>% 
    summarize(ntags=length(unique(tagid)))
  
  summ_tb_tmp_w1 <- summ_tb_tmp1 %>% tidyr::pivot_wider(names_from=c(trans_stat,event),values_from = ntags)
  
  
  summ_tb_tmp2 <- tags_comb_raw_in %>% 
    group_by(esutype,reartype,rel_year,event,rel_at_LGR,AVIAN_recov) %>% 
    summarize(ntags=length(unique(tagid)))
  
  summ_tb_tmp_w2 <- summ_tb_tmp2 %>% tidyr::pivot_wider(names_from=c(AVIAN_recov,event),values_from = ntags)
  

  summ_tb_tmp3 <- tags_comb_raw_in %>%
    mutate(excluded=!(trans_statTF|tags_comb_raw$nonLGR_intra_dam_rel),
           ever_seen=dets>0 | AVIAN_recov) %>%
    group_by(esutype,reartype,rel_year) %>% 
    summarize(tot_rel=length(unique(tagid)),
              seen_post_rel=sum(ever_seen),
              seen_post_rel_FOC=sum(AVIAN_recov | prim_loc_det),
              det_post_rel=sum(prim_loc_det),
              recov_post_rel=sum(AVIAN_recov),
              only_recov_post_rel=sum(AVIAN_recov & !prim_loc_det),
              rel_at_LGR=sum(rel_at_LGR),
              LGR_loc_det=sum(LGR_loc_det),
              MCN_loc_det=sum(MCN_loc_det)
              ) %>%
    select(esutype,reartype,rel_year,tot_rel,seen_post_rel,seen_post_rel_FOC,det_post_rel,recov_post_rel,only_recov_post_rel,rel_at_LGR,LGR_loc_det,MCN_loc_det)  
  
  
  summ_tb_tmp4 <- tags_comb_raw_in %>%
    mutate(excluded=(trans_statTF|tags_comb_raw$nonLGR_intra_dam_rel),
           ever_seen=dets>0 | AVIAN_recov) %>%
    group_by(esutype,reartype,rel_year) %>% 
    summarize(tot_rel=length(unique(tagid)),
              
              # seen_post_rel=sum(ever_seen),
              seen_post_rel_FOC=sum(AVIAN_recov | prim_loc_det),
              seen_post_rel_FOC_exclu=sum((AVIAN_recov | prim_loc_det) & excluded),
              seen_post_rel_FOC_notexclu=sum((AVIAN_recov | prim_loc_det) & !excluded),
              # det_post_rel=sum(prim_loc_det),
              # recov_post_rel=sum(AVIAN_recov),
              # only_recov_post_rel=sum(AVIAN_recov & !prim_loc_det),
              rel_at_LGR=sum(rel_at_LGR & !excluded),
              LGR_loc_det=sum(LGR_loc_det & !excluded),
              MCN_loc_det=sum(MCN_loc_det & !excluded)
              
    ) #%>%
    # select(esutype,reartype,rel_year,tot_rel,seen_post_rel,seen_post_rel_FOC,det_post_rel,recov_post_rel,only_recov_post_rel,rel_at_LGR,LGR_loc_det,MCN_loc_det)  
  
  
  
  # filtering out trans_statTF=FALSE
  summ_tb_tmp5 <- tags_comb_raw_in %>%
    mutate(excluded=!(trans_statTF|tags_comb_raw$nonLGR_intra_dam_rel),
           ever_seen=dets>0 | AVIAN_recov) %>%
    filter(excluded) %>% # mirrors 02c code
    group_by(esutype,reartype,rel_year) %>% 
    summarize(tot_rel=length(unique(tagid)),
              seen_post_rel=sum(ever_seen),
              seen_post_rel_FOC=sum(AVIAN_recov | prim_loc_det),
              det_post_rel=sum(prim_loc_det),
              recov_post_rel=sum(AVIAN_recov),
              only_recov_post_rel=sum(AVIAN_recov & !prim_loc_det),
              rel_at_LGR=sum(rel_at_LGR),
              LGR_loc_det=sum(LGR_loc_det),
              MCN_loc_det=sum(MCN_loc_det)
    ) #%>%
    # select(esutype,reartype,rel_year,tot_rel,seen_post_rel,seen_post_rel_FOC,det_post_rel,recov_post_rel,only_recov_post_rel,rel_at_LGR,LGR_loc_det,MCN_loc_det)  
  
  
  
  out <- list(
          "summ_tb_tmp1"=summ_tb_tmp1,
          "summ_tb_tmp_w1"=summ_tb_tmp_w1,
          "summ_tb_tmp2"=summ_tb_tmp2,
          "summ_tb_tmp_w2"=summ_tb_tmp_w2,
          "summ_tb_tmp3"=summ_tb_tmp3,
          "summ_tb_tmp4"=summ_tb_tmp4,
          "summ_tb_tmp5"=summ_tb_tmp5
          
          )
  
}



tag_filt_plt <- function(){
  
  
  
}




