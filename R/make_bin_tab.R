
make_bin_tab <- function(bin_ls=bin_list,
                         row_data,strt,end,interv){
  par_last=FALSE
  labs=c("day","days3.5","week1","weeks2","month")
  seq_bin_size=paste(24*c(1,3.5,7,14,28),"hours")
  bin_hours=24*c(1,3.5,7,14,28)
  ind=match(interv,labs)
  
  # sequence created to see how long that it is b/c seq() will stop short of last one
  xx=seq(off_used$off_strt[ii],off_used$off_end[ii],seq_bin_size[ind])
  nn=length(xx)
  par_vec=FALSE#rep(FALSE,length(xx))#,TRUE)
  
  # handling of partial bin
  # if the end of the last bin goes beyond the middle 99% quantile, 
  # then one should note that the number of hours by which the final bin overshot
  if(nn==1){
    
    par_vec=TRUE
    bin_strt=xx[1]
    bin_end=bin_strt + lubridate::hours(bin_hours[ind])
    ind_bin_hours <- as.numeric(difftime(off_used$off_end[ii],off_used$off_strt[ii],units = "hours"))
    supp_cols <- data.frame(row_data[1,])
    binID <- 1
  } else{
    
    ind_bin_hours=rep(as.numeric(bin_hours[ind]),nn-1) #    # defaults to bin size
    bin_strt <- xx[1:(nn-1)]
    bin_end <- xx[2:nn]
    
    if(xx[nn]<off_used$off_end[ii]){
      xx <- c(xx,xx[nn]+lubridate::hours(bin_hours[ind])) # adds an extra bin which will include last ob
      nn <- length(xx)
      
      bin_end <- c(bin_end,xx[length(xx)])
      bin_strt <- c(bin_strt,xx[length(xx)-1])
      #in this case, important to use length(xx) rather than 'nn' b/c xx has been altered
      ind_bin_hours <- c(ind_bin_hours,as.numeric(difftime(xx[length(xx)], 
                                                           off_used$off_end[ii],units = "hours")))
      par_vec=ind_bin_hours!=bin_hours[ind]}
    
    binID <- (1:(length(xx)-1))
    supp_cols <- row_data[rep(1,length(xx)-1),]
  }
  
  
  
  out=data.frame(supp_cols,
                 binID,
                 bin=labs[ind],
                 official_strt=off_used$off_strt[ii],
                 official_end=off_used$off_end[ii],
                 bin_strt=bin_strt,
                 bin_end=bin_end,
                 partial=par_vec,
                 # actual bin size
                 ind_bin_hours=ind_bin_hours,
                 ind_bin_days=ind_bin_hours/24,
                 # expected bin size if full
                 full_bin_hours=bin_hours[ind])
  out
}