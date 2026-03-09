##################### #
# TABLE 36
##################### #

# pasting into excel, saving as a .csv and then importing
matrix(unlist(read.csv("C:/Users/swhit/Downloads/Widener_2025_tb36_pst.csv",header = F)),byrow=T,ncol=5)

dir("C:/Users/swhit/Downloads",pattern="Wide")
