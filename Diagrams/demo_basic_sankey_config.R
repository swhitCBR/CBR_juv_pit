
library(dplyr)
library(ggplot2)
library(ggsankey)


sankey_subsetting_comp_fun  <- function(
  tot_rel=1000,
  exclude_v=c(0,300,200,200,50,100,50),
  stage=c("I","II","III","IV","V","VI","VII"),
  init_lab=c("Release"),
  retain_labs=c("Seen","Retained","Not Transported","Prim_Loc","LGR or MCN","Within"),
  exclude_labs=c("Never Seen","Only Avian Recovery","Transported","Not Prim_Loc","Not LGR or MCN","Not Within")) 
{
    # exclude_v <- c(0,300,200,200,50,100,50)
    prev_exclude_v <- c(0,cumsum(exclude_v[1:length(exclude_v)-1]))
    retain_v <- tot_rel-exclude_v-prev_exclude_v

    mat_sank <- data.frame(retain_labs=c(init_lab,retain_labs),exclude_labs=c(NA,exclude_labs),retain_v,exclude_v,prev_exclude_v)
    
    sank_ls_tmp <- list()
    for(ii in 1:nrow(mat_sank)){
      if(ii==1){
        sank_ls_tmp[[ii]] <- rep(init_lab,mat_sank$retain_v[ii])
      } else{
        sank_ls_tmp[[ii]] <- c(rep(mat_sank$retain_labs[ii],retain_v[ii]),
                               rep(mat_sank$exclude_labs[ii],exclude_v[ii]),
                               rep(NA,prev_exclude_v[ii]))
      }
    }
    d_test <- data.frame(id=1:1000,do.call(cbind,sank_ls_tmp))
    names(d_test) <- c('id',stage)
    d <- d_test
    
}

sankey_plotting  <- function(sankeyDF_in){
  
  
  # Step 1
  df <- sankeyDF_in %>%
    make_long(I, II, III,IV,V,VI,VII) #%>%  filter(!is.na(next_node)
  
  # Step 2
  dagg <- df%>%
    dplyr::group_by(node)%>%
    tally()
  
  # Step 3
  df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE) %>% 
    filter(!(x=="III" & is.na(node)) & 
             !(x=="IV" & is.na(node)) &
             !(x=="V" & is.na(node)) &
             !(x=="VI" & is.na(node)) &
             !(x=="VII" & is.na(node))) 
  
  
  # refactoring levels for plot
  fact_levs1 <- c("Never Seen","Seen","Transported","Not Transported","Not Within","Within","Only Avian Recovery","Retained",
                  "Not LGR or MCN","LGR or MCN","Not Prim_Loc","Prim_Loc",
                  "Release")
  fact_levs2 <- c("Never Seen","Seen","Transported","Not Transported","Not Within","Within","Only Avian Recovery","Retained",
                  "Not LGR or MCN","LGR or MCN","Not Prim_Loc","Prim_Loc")
  
  df2 <-df2 %>% mutate(
    node=factor(node,levels=fact_levs1),
    next_node=factor(next_node,levels=fact_levs2))
  
  
  head(df2)
  df2$omitted <- df2$next_node %in% c("Never Seen","Transported","Not Within")
  
  # paste0("c('",paste(as.character(unique(df2$node)),collapse="','"),"')")
  c('LGR or MCN',
    'Never Seen',
    'Not LGR or MCN',
    'Not Prim_Loc',
    'Not Transported',
    'Not Within',
    'Only Avian Recovery',
    'Prim_Loc','Release',
    'Retained',
    'Seen',
    'Transported',
    'Within'
  )
  
  ggplot(data=df2, aes(x = x
                       , next_x = next_x
                       , node = node
                       # , fill = node,
                       , next_node = next_node
                       , label = paste0(node," n=", n))) +
    geom_sankey(flow.alpha = 0.5) +
    geom_sankey_label(
      # alpha = 0.4,
      size = 3) + 
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.title = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks = element_blank()  
          , panel.grid = element_blank())
  
}


str(example_sankeyDF) 
# columns equal total tags
# initial label and then strings with the name of excluded and retained groups
# stage is the number of columns (excluding the id)


example_sankeyDF <- sankey_subsetting_comp_fun(
                        tot_rel=5000,
                        exclude_v=c(0,2500,200,200,50,100,50),
                        stage=c("I","II","III","IV","V","VI","VII")) 


sankey_plotting(sankeyDF_in = example_sankeyDF)



#++
  # scale_fill_manual(
  #   values = c(
  #     'LGR or MCN'= "cadetblue",
  #     'Never Seen'= "darkred",
  #    'Not LGR or MCN' = "cadetblue",
  #    'Not Prim_Loc'   = "cadetblue",
  #    'Not Transported' = "pink",
  #    'Not Within' = "#2980b9",
  #    'Only Avian Recovery' = "violet",
  #    'Prim_Loc','Release' = "cadetblue",
  #    'Retained' = "goldenrod",
  #    'Seen' = "darkred",
  #    'Transported' = "darkblue",
  #    'Within' = "#D3D3D3"
  # ), na.value = "orange")





ggplot() +  geom_sankey(
  data=df2, aes(x = x
                , next_x = next_x
                , node = node
                , next_node = next_node
                , fill = factor(node==node)
                , label = paste0(node," n=", n)),
  flow.alpha = 0.5) +
  geom_sankey_label(
      data=df2, aes(x = x
                , next_x = next_x
                , node = node
                , next_node = next_node
                , fill = factor(node==node)
                , label = paste0(node," n=", n)),
      size = 3, color = "white") + theme(legend.position = "none")









# Chart 2
pl <- ggplot(
  data=df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      # , fill = omitted
                      , fill = factor(node==node)
                      # , color = factor(node=="Survived")
                      , label = paste0(node," n=", n)
)
) 
# pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
# pl <- pl +geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)

pl <- pl +geom_sankey(flow.alpha = 0.5)#,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "white")#, fill= "gray40", hjust = -0.2)


pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
# pl <- pl + scale_fill_viridis_d(option = "viridis")
# pl <- pl + scale_fill_manual(values = "gray20",na.value = "white")
# pl <- pl + scale_color_manual(values = "gray20",na.value = "white")
pl <- pl + labs(title = "Tag Filtering Steps")
# pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
# pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')


pl

