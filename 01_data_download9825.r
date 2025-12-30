
# in_out_dir="C:/Users/17072/OneDrive - UW/Desktop/data"

in_out_dir="temp/data_9825"
dir.create(in_out_dir,recursive = T)
dir.exists(in_out_dir)


bt_all <- proc.time()

# Downloading Snake River Chinook data 
rel_yrs=1996:2025

bt <- proc.time()
bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Ch1_",YYYY,".csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt


bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Sthd_",YYYY,".csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt

# THIS ERROR'S OUT N 2025
bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Sock_",YYYY,".csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt



############################################### #
in_out_dir="temp/data_9825_REC"
dir.create(in_out_dir)
dir.exists(in_out_dir)
############################################### #

############################################### #
# Downloading Snake River Recovery(?) records
############################################### #

bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Ch1_",YYYY,"_REC.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt



bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Sthd_",YYYY,"_REC.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt


bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Sock_",YYYY,"_REC.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt

ii


############################################### #
in_out_dir="temp/data_9825_TWX"
dir.create(in_out_dir)
dir.exists(in_out_dir)
############################################### #

# Downloading Snake River Chinook data 
# rel_yrs=1996:2023

bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Ch1_",YYYY,"_TWX.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt



bt <- proc.time()
for(ii in 1:length(rel_yrs)){
  YYYY=rel_yrs[ii]
  fl_nm=paste0("SR_Sthd_",YYYY,"_TWX.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt


############################################# #
# important! some of these years are missing
############################################# #

bt <- proc.time()
# rel_yrs[ind]
ind <- c(1:4,7:24,26:length(rel_yrs)) # gets rid of 2000 releases
rel_yrs[25]
for(ii in 1:length(ind)){
  YYYY=rel_yrs[ind[ii]]
  fl_nm=paste0("SR_Sock_",YYYY,"_TWX.csv")
  my_url=paste0("www.cbr.washington.edu/dart/cs/data/west_esu_detect/",fl_nm)
  download.file(my_url,destfile=file.path(in_out_dir,fl_nm))
}
proc.time()-bt


proc.time()-bt_all



# www.cbr.washington.edu/dart/cs/data/west_esu_detect/SR_Ch1_2007.csv
# www.cbr.washington.edu/dart/cs/data/west_esu_detect/SR_Sock_2007.csv