
#### loading all location and activity data updated on 1-9-2023 ####
library(foreign)
library(dplyr)


# # Website
# https://www.ptagis.org/Sites/Map
#   # Download Geodatabase

# loading interrogation site data 
# INT_sites <- read.dbf("C:/repos/SR_PIT_det_trends/temp/PTAGIS_INT_SITE.dbf")
INT_sites <- read.dbf("temp/PTAGIS_INT_Sites.dbf")


# Reference files from last year
fls <- c("relsite2hydro_unit.csv","rel_site_ref_mod.csv","rel_site_locs.csv")#,"inter_site_key.csv")
out <- c(sapply(file.path("temp/2024_files",fls),read.csv),list(INT_sites))
names(out) <- c("relsite2hydrounit","relsite_ref","rel_site_locs")#,"inter_site_key","INT_sites")
# dataframes with detailed interrogation an release site information
int_site_pos <- out$INT_sites
rel_site_pos <- out$rel_site_locs


# # Website
# https://www.ptagis.org/Sites/Map
#   # Download Geodatabase
#   https://www.ptagis.org/Sites/Map?handler=FindFile
#   # MRR sites CSV
#   https://ptagisbi.ptagis.org/PTAGIS/asp/Main.aspx?Server=THANOS&Project=PTAGIS+BI&Port=0&connMode=8&src=Main.aspx.3067&evt=3067&reportID=1158A21048EC1BEF8C9339B001720AF1&reportViewMode=1&executionMode=4&showOptionsPage=false
#   # interrogation sites CSV
#   https://ptagisbi.ptagis.org/PTAGIS/asp/Main.aspx?Server=THANOS&Project=PTAGIS+BI&Port=0&connMode=8&src=Main.aspx.3067&evt=3067&reportID=0F8733314ECEAB853633E2A2B1A03AA8&reportViewMode=1&executionMode=4&showOptionsPage=false
# 

MRR_sites <- data.frame(readxl::read_xlsx("temp/PITAGIS_dart_INT_MRR_12-23-25.xlsx",sheet = "PTAGISMRRSites"))
Int_sites_ant  <- data.frame(readxl::read_xlsx("temp/PITAGIS_dart_INT_MRR_12-23-25.xlsx",sheet = "DART_PIT Tag Interrogation Site"))
Intra_dam_codes  <- data.frame(readxl::read_xlsx("temp/PITAGIS_dart_INT_MRR_12-23-25.xlsx",sheet = "Intra-Dam Release Site Codes"))

# SUPLEMENTING INT_sites with PD5, PD6, and PD8 information
# https://www.ptagis.org/Sites/InterrogationSites?code=PD5
supp_pds <-   data.frame(SiteType="Instream Remote Detection System",
                         SiteCode=c("PD5","PD6","PD8"),
                         RKM=c("062","068","0.082"),
                         matrix(
                           c(46.205748, -123.431179,
                             46.152328, -123.385374,
                             46.166489, -123.225002),
                           ncol=2,byrow=T,dimnames = list(NULL,c("Latitude","Longitude"))))

INT_sites$RKM <- as.character(INT_sites$RKM)
INT_sites2 <- bind_rows(INT_sites,supp_pds)

# # these are no good and appear to be relocations
# INT_sites2[INT_sites2$SiteType=="Monitored Fish Release",]

### Generating all possible intra-dam release codes

aa=Int_sites_ant[Int_sites_ant$Site.Type=="dam",]$DART.Location.Code
bb=aa[!is.na(aa)]
supp_dam_nms <- c("LGR","LMO","LGO")
supp_dam_nms[!supp_dam_nms %in% bb]
cc <- c(bb,supp_dam_nms)
eg_combs=data.frame(expand.grid(cc,Intra_dam_codes$Code))

# potential intra-dam codes
eg_combs$pot_codes <- paste0(eg_combs$Var1,eg_combs$Var2)
pot_intradam_codes <- eg_combs$pot_codes

# all intradam codes that would disqualify fish from inclusion in the data set except for LGRRRR
codes_non_lgr_intradam_codes <- pot_intradam_codes[!pot_intradam_codes %in% c("LGRRRR")]

# unacceptable
codes_non_lgr_intradam_codes[grep(codes_non_lgr_intradam_codes,pattern="LGR")]


#### Specifying primary observation sites ####

prim_obssites <- c(
  # LGR spillway detection
  INT_sites2[INT_sites2$SiteType=="Spillway" & INT_sites2$SiteCode %in% c("GRS"),]$SiteCode,
  # nonexperimental JBSs
  INT_sites2[INT_sites2$SiteType=="Juvenile Fish Bypass Facility" & INT_sites2$SiteCode %in% c("GRJ","GOJ","LMJ","MCJ","JDJ","B2J","BCC"),]$SiteCode, #"B1J","BVX","BVJ"
  INT_sites2[INT_sites2$SiteType=="Trawl Net",]$SiteCode,
  INT_sites2[INT_sites2$SiteType=='Instream Remote Detection System' & INT_sites2$SiteCode %in% c("PD5","PD6","PD7","PD8"),]$SiteCode,
  # Ice harbor combined
  INT_sites2[ INT_sites2$SiteCode %in% c("ICH"),]$SiteCode#,
  # INT_sites2[ INT_sites2$SiteCode %in% c("GRX","MCX"),]$SiteCode # experimental receivers that were active around the time of the changeover to GRJ and MCJ, respectively
)

length(prim_obssites)

prim_obssiteDF <- data.frame(obssite=prim_obssites)
prim_obssiteDF
prim_obssiteDF$loc_cat <- c("LGR","BON","BON","LGS","LGR","JDA","LMN","MCN",rep("Estuary",9),"ICH")#[1:18]# added later

prim_obssiteDF

