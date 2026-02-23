
library(dplyr)

# from "04a_recomp_to_rep_m_or_b_scripts98_25_wPD568"
# lgr_dh_tab2 <- readRDS("comp_files/lgr_dh_tab2_9825_wPD568.rds")
# lgr_est_outDF2 <- readRDS("temp/lgr_est_outDF2.rds")
lgr_est_outDF3 <- readRDS("temp/lgr_est_outDF3.rds")

table(lgr_est_outDF3$esutype)
head(lgr_est_outDF3)

# lgr_est_outDF3 %>%   filter(esutype=="SR_Ch1" & reartype =="H" & aggre_lev=="year") #%>% 
# adding SE in parentheses to estimates (kind of sloppy rounding)
# subb0 <- lgr_est_outDF3 %>% filter(esutype=="SR_Sthd" & reartype =="H" & aggre_lev=="month") #%>% 
# subb1 <- subb0[grep(subb0$code3,pattern="lgr_pooled"),]
# lgr_est_outDF3$S1_werr <- paste0(round(lgr_est_outDF3$s1,2),"(",round(sqrt(lgr_est_outDF3$s1_var),2),")")
# lgr_est_outDF3$S1_werr[lgr_est_outDF3$S1_werr=="NaN(NaN)"] <- "--"
# lgr_est_outDF3$S1_werr[lgr_est_outDF3$S1_werr=="Inf(NaN)"] <- "--"

lgr_est_outDF3 %>% filter(esutype=="SR_Ch1" & defin_det_yr==2003 & aggre_lev == "year" & reartype=="W") 


lgr_est_outDF3 %>% filter(esutype=="SR_Ch1" & defin_det_yr==2003 & aggre_lev == "month" & reartype=="W") 


tmp <- lgr_est_outDF3 %>% 
  select(esutype,reartype,dat_grp,aggre_lev,defin_det_yr,N,n.11) %>%  
  filter(aggre_lev=="year" & reartype=="W") %>%
  tidyr::pivot_wider(values_from = c(n.11,N),names_from = dat_grp) %>%
  mutate(addl_n.11=n.11_lgr_pooled-n.11_lgr_det)

tmp$addl_n.11




source("C:/repos/CBR_juv_pit/R/LGR_summ_tab_fns.R")
devtools::load_all("C:/repos/repo_simCJS/simCJS")

get_est_tab(head(comb_wide_DF))

# wide format with S1_est across actual detections vs. releases
LGR_summ_tab_fn1(lgr_est_tb_in = lgr_est_outDF3)

SEP_RT_lgr_annDF=LGR_COMB_rt_summ1(lgr_est_outDF3)
# SEP_RT_lgr_annDF=data.frame(ddd,round(get_est_tab(ddd),3)) %>%
#   # mutate(dat_grp="lgr_pooled",reartype="COMB") %>%
#   relocate(esutype,reartype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)

COMB_lgr_ann_ls=LGR_COMB_rt_summ(lgr_est_outDF3)
# data.frame(COMB_lgr_ann_ls$"det",round(get_est_tab(COMB_lgr_ann_ls$"det"),3)) %>% filter(esutype=="SR_Ch1")
# data.frame(COMB_lgr_ann_ls$"pool",round(get_est_tab(COMB_lgr_ann_ls$"pool"),3)) %>% filter(esutype=="SR_Ch1")

# within year comparison with different pooling and detections
comb_wide_DF <- bind_rows(
          data.frame(SEP_RT_lgr_annDF,round(get_est_tab(SEP_RT_lgr_annDF),3)),
          data.frame(COMB_lgr_ann_ls$"det",round(get_est_tab(COMB_lgr_ann_ls$"det"),3)),
          data.frame(COMB_lgr_ann_ls$"pool",round(get_est_tab(COMB_lgr_ann_ls$"pool"),3))) %>%
  arrange(esutype,defin_det_yr)  %>% mutate(code=paste(dat_grp,reartype)) %>% filter(esutype=="SR_Ch1" & code != "lgr_pooled H")


head(comb_wide_DF)

# write.csv(comb_wide_DF,"temp/comb_wide_DF.csv",row.names=F)
# View(comb_wide_DF %>% arrange(defin_det_yr) %>% filter(esutype=="SR_Ch1"))
View(comb_wide_DF)



source("C:/repos/repos/R/LGR_summ_tab_fns.R")
devtools::load_all("C:/repos/repo_simCJS/simCJS")

# get_est_tab <- function(df_in){
#   cc_col_ind <- match(c("n.11","n.01","n.10","n.00"),names(df_in))
#   cc_ls <- apply(df_in[,cc_col_ind],1,function(x){x},simplify=F)
#   t(sapply(cc_ls,function(xx){per2_surph_ests(cell_vals_in = xx,SE=T,w_table = F)}))
# }
# 
# 
# per2_surph_ests(cell_vals_in = cc_ls[[1]],SE=F)
# per2_surph_ests(cell_vals_in = cc_ls[[1]],SE=T,w_table = F)


# if length of per1_surph_ests() 1 then table by default,
# if length >1 then table option is disabled
# maybe argument for type of output or input and a message





est_tab <- t(sapply(1:nrow(sim_counts),function(jj){
  cell_vals <- sim_counts[jj,]
  names(cell_vals) <- c("n.11","n.01","n.10","n.00")
  per2_surph_ests(cell_vals,SE=F)}))





cc %>% select(-lgr_det) %>% 
  tidyr::pivot_wider(names_from=name,values_from = lgr_pool) %>%
  mutate(dat_grp="lgr_pool") %>%
  relocate(esutype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)


cc %>% select(-lgr_pooled) %>%
  tidyr::pivot_wider(names_from=name,values_from = lgr_det) %>%
  mutate(dat_grp="lgr_det") %>%
  relocate(esutype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)

aa

aa %>% group_by(esutype,dat_grp,defin_det_yr,name) %>%
  summarize(value=sum(value))
  

relocate(esutype,dat_grp,defin_det_yr,N,n.11,n.10,n.01,n.00)



aa %>% group_by(esutype,dat_grp,defin_det_yr,name) %>%
  summarize(value=sum(value)) %>% 
  ungroup() %>%
  tidyr::pivot_wider(names_from=c(dat_grp,name),values_from=value)



  # summarize(N=sum(N),
  #           n.11=sum(n.11),
  #           n.10=sum(n.10),
  #           n.01=sum(n.01),
  #           n.00=sum(n.00))



# lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled") %>% select(esutype,reartype,defin_det_yr,S1_werr) #%>% #tidyr::pivot_wider(values_from = )
# ann_estsTAB <- lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled" & aggre_lev=="year") %>% 
#   select(defin_det_yr,esutype,reartype,N,S1_werr)  %>% tidyr::pivot_wider(names_from = c(esutype,reartype),values_from = c(S1_werr,N))
# write.csv(ann_estsTAB,"ann_estsTAB.csv",row.names = F)



# adding cell counts
lgr_est_outDF3_ALT <- lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled") %>% select(esutype,reartype,defin_det_yr) #%>% #tidyr::pivot_wider(values_from = )

lgr_est_outDF3

# ann_estsTAB <- lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled" & aggre_lev=="year") %>% 
#   select(defin_det_yr,esutype,reartype,N,S1_werr)  %>% tidyr::pivot_wider(names_from = c(esutype,reartype),values_from = c(S1_werr,N))





ann_estsTAB_all_spp_det <- lgr_est_outDF3 %>% filter(dat_grp=="lgr_det" & aggre_lev=="year") %>%
  select(dat_grp,esutype,reartype,defin_det_yr,s1,s1_var,N) %>% #pivot_longer()#%>% #tidyr::pivot_wider(values_from = )
  mutate(S1_est=round(s1,3),
         S1_se=round(sqrt(s1_var),3)) %>%
  select(-s1,-s1_var) %>%
  tidyr::pivot_longer(cols = c(N,S1_est,S1_se))

ann_estsTAB_all_spp_pooled <- lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled" & aggre_lev=="year") %>%
  select(dat_grp,esutype,reartype,defin_det_yr,s1,s1_var,N) %>% #pivot_longer()#%>% #tidyr::pivot_wider(values_from = )
  mutate(S1_est=round(s1,3),
         S1_se=round(sqrt(s1_var),3)) %>%
  select(-s1,-s1_var) %>%
  tidyr::pivot_longer(cols = c(N,S1_est,S1_se))

ann_estsTAB_rev1_det <- ann_estsTAB_all_spp_det %>% filter(esutype=="SR_Ch1") %>%
  tidyr::pivot_wider(names_from = c(reartype,name),values_from = c(value))

ann_estsTAB_rev1_pool <- ann_estsTAB_all_spp_pooled %>% filter(esutype=="SR_Ch1") %>%
  tidyr::pivot_wider(names_from = c(reartype,name),values_from = c(value))

ann_estsTAB_rev1_det
ann_estsTAB_rev1_pool

write.csv(ann_estsTAB_rev1_det,"temp/ann_estsTAB_rev1_det.csv",row.names = F)
write.csv(ann_estsTAB_rev1_pool,"temp/ann_estsTAB_rev1_pool.csv",row.names = F)





write.csv(ann_estsTAB_rev1,"temp/ann_estsTAB_rev1.csv",row.names = F)


paste0(round(lgr_est_outDF3$s1,2),"(",round(sqrt(lgr_est_outDF3$s1_var),2),")")




# lgr_est_outDF3 %>% filter(dat_grp=="lgr_pooled") %>% select(esutype,reartype) %>% tidyr::pivot_wider(id_cols = 1:2)
