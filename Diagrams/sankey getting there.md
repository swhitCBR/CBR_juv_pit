
> [!NOTE]- initial version
> 
> ```
> library(ggplot2)
> library(ggsankeyfier)
> library(tidyr)
> library(dplyr)
> 
> # 1. Setup data
> df_wide <- data.frame(
>   Stage1 = rep("Released", 8),
>   Stage2 = c("LGR", "LGR", "Never detected", "Never detected", "MCN", "MCN","BON","BON"),
>   Outcome = c("Success", "Fail", NA, NA, "Success", "Fail","Success", "Fail"), 
>   Impact = c("High", "Low", NA, NA, "High", "High", "Low", "Low"),
>   Value = c(10, 20, 40, 10, 15, 5, 5, 5)
> )
> 
> # 2. Pivot to long format
> df_pivot <- pivot_stages_longer(
>   data = df_wide, 
>   stages_from = c("Stage1", "Stage2", "Outcome", "Impact"), 
>   values_from = "Value"
> )
> 
> # 3. Manually control edge colors
> df_pivot <- df_pivot %>%
>   mutate(edge_fill = ifelse(stage == "Stage1", "Initial_Flow", node))
> 
> # 4. Positioning for top alignment
> pos <- position_sankey(align = "top", v_space = 0.05)
> 
> # 5. Generate the plot
> ggplot(df_pivot, aes(x = stage, y = Value, group = node, connector = connector, edge_id = edge_id)) +
>   geom_sankeyedge(aes(fill = edge_fill), position = pos, alpha = 0.4) +
>   geom_sankeynode(aes(fill = node), position = pos) +
>   geom_text(
>     data = subset(df_pivot, !node %in% c("Success", "Fail", "High", "Low") & !is.na(node)), 
>     stat = "sankeynode", aes(label = node), 
>     position = pos, vjust = -0.5, fontface = "bold"
>   ) +
>   scale_fill_manual(values = c(
>     "Released"       = "cadetblue",
>     "Never detected" = "green",
>     "LGR"            = "#2980b9",
>     "MCN"            = "#8e44ad",
>     "BON"            = "violet",
>     "Success"        = "magenta",
>     "Fail"           = "goldenrod",
>     "High"           = "#d35400",
>     "Low"            = "#27ae60",
>     "Initial_Flow"   = "#D3D3D3"
>   ), na.value = NA) +
>   
>   # UPDATE X-AXIS LABELS HERE
>   scale_x_discrete(labels = c("Stage1" = "Release", 
>                               "Stage2" = "Initial Detection", # Changed from "Detection"
>                               "Stage3" = "Result", 
>                               "Stage4" = "Impact")) +
>   
>   theme_minimal() +
>   theme(legend.position = "none") +
>   labs(title = "4-Stage Sankey with Updated Axis Labels", x = "Process Stage", y = "Total")
> ```


```
library(ggplot2)
library(ggsankeyfier)
library(tidyr)
library(dplyr)

# 1. Setup data
df_wide <- data.frame(
  Stage1 = rep("Released", 8),
  Stage2 = c("LGR - detected", "LGR - detected", "Never detected", "Never detected", 
             "LGR - not detected", "LGR - not detected", 
             "LGR - not detected", "LGR - not detected"),
  BON_Stage = c("MCN - detected", "MCN - not detected", NA, NA, 
                "MCN - detected", "MCN - not detected", 
                "MCN - detected", "MCN - not detected"), 
  Impact = c("High", "Low", NA, NA, "High", "High", "Low", "Low"), 
  Value = c(10, 20, 40, 10, 15, 5, 5, 5)
)

# 2. Pivot to long format
df_pivot <- pivot_stages_longer(
  data = df_wide, 
  stages_from = c("Stage1", "Stage2", "BON_Stage", "Impact"), 
  values_from = "Value"
)

# 3. Create helper for gray connections from 'Released'
df_pivot <- df_pivot %>%
  mutate(edge_fill = ifelse(stage == "Stage1", "Initial_Flow", node))

# 4. Positioning for top alignment
pos <- position_sankey(align = "top", v_space = 0.05)

# 5. Generate the plot
ggplot(df_pivot, aes(x = stage, y = Value, group = node, connector = connector, edge_id = edge_id)) +
  
  # Connectors (Edges)
  geom_sankeyedge(aes(fill = edge_fill), position = pos, alpha = 0.4) +
  
  # Boxes (Nodes)
  geom_sankeynode(aes(fill = node), position = pos) +
  
  # SHOW ALL LABELS: Removed the %in% exclusion filter
  geom_text(
    data = subset(df_pivot, !is.na(node)), 
    stat = "sankeynode", 
    aes(label = node), 
    position = pos, 
    vjust = -0.8, # Adjusted slightly higher to avoid overlapping boxes
    size = 3,
    fontface = "bold"
  ) +
  
  # Manual Color Definition
  scale_fill_manual(values = c(
    "Released"           = "cadetblue",
    "Never detected"     = "green",
    "LGR - detected"     = "#2980b9",
    "LGR - not detected" = "violet",
    "MCN - detected"     = "magenta",
    "MCN - not detected" = "goldenrod",
    "High"               = "darkred",
    "Low"                = "darkblue",
    "Initial_Flow"       = "#D3D3D3"
  ), na.value = NA) +
  
  # X-axis labels
  scale_x_discrete(labels = c("Stage1" = "Release", 
                              "Stage2" = "Initial Detection", 
                              "Stage3" = "BON", 
                              "Stage4" = "Impact")) +
  
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(title = "Full Sankey Diagram with All Node Labels", x = "Process Stage", y = "Count")
```
