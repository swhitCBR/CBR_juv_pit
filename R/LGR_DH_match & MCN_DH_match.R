

# LGR
LGR_DH_match <- data.frame(DH_label=c(
  "LGR",
  "LGR -> BON",
  "LGR -> BON -> Estuary",
  # "LGR -> BON -> MCN" not acceptable, convert 1010
  "LGR -> Estuary",
  "LGR -> MCN",
  "LGR -> MCN -> BON",
  "LGR -> MCN -> BON -> Estuary",
  "LGR -> MCN -> Estuary",
  "LGR -> BON -> MCN"),
  DH_code=sapply(list(
    c(1,0,0,0),
    c(1,0,1,0),
    c(1,0,1,1),
    c(1,0,0,1),
    c(1,1,0,0),
    c(1,1,1,0),
    c(1,1,1,1),
    c(1,1,0,1),
    c(1,0,0,1)),paste,collapse=","))

int_tab=do.call(rbind,sapply(LGR_DH_match$DH_code,strsplit,split=","))
rownames(int_tab)=NULL
colnames(int_tab)=c("LGR","MCN","BON","Estuary")
LGR_DH_matchDF <- cbind(LGR_DH_match,int_tab)


MCN_DH_match <- data.frame(DH_label=c(
  "MCN",
  "MCN -> BON",
  "MCN -> BON -> Estuary",
  "MCN -> Estuary"),
  DH_code=sapply(list(
    c(1,0,0),
    c(1,1,0),
    c(1,1,1),
    c(1,0,1)),paste,collapse=","))

int_tab=do.call(rbind,sapply(MCN_DH_match$DH_code,strsplit,split=","))
rownames(int_tab)=NULL
colnames(int_tab)=c("MCN","BON","Estuary")
MCN_DH_matchDF <- cbind(MCN_DH_match,int_tab)

rm(int_tab,MCN_DH_match,LGR_DH_match)

