
get_ests_from_MSS <- function(cell_vals_in,type="Skalski 1998"){
  
  skals98 <- per2_skalski_ests(cell_vals_in)
  
  surph <- per2_surph_ests(cell_vals_in)
  
  out=list(skals98,surph)
  
  
}



per2_skalski_ests <- function(cell_vals_in){
  
  R1<-sum(cell_vals_in)
  R2<-sum(cell_vals_in[c("n.11","n.10")])
  
  r1<-sum(cell_vals_in[c("n.11","n.01","n.10")])
  r2<-sum(cell_vals_in["n.11"])
  
  m12=sum(cell_vals_in[c("n.11","n.10")])
  m13=sum(cell_vals_in[c("n.01")])
  
  z2=sum(cell_vals_in[c("n.01")])
  T2=m12+z2
  
  # correct S1 Skalski 1998 w/ indices shifted down
  # S1=r1/R1*((m12/T2)+((z2*R2)/(T2*r2)) )
  
  marr=matrix("",nrow=7,ncol=7)
  marr[,1]=c("i  ","","1  ","2  ","","m[j]","z[j]")
  marr[,2]=c("R[i]","",R1,R2,"","","")
  marr[,4]=c("j=2","",m12,"","",m12,z2)
  marr[,5]=c("j=3","",m13,"","",m13,"")
  marr[,7]=c("r[i]","",r1,r2,"","","")
  
  rownames(marr)=NULL
  colnames(marr)=NULL
  
  tmp <- as.data.frame(marr)
  names(tmp)=rep("",7)
  cat("m-array table")
  print(tmp,row.names = F,quote = F)
  # correct P1 Skalski 1998 w/ indices shifted down
  
  par_ls <- list(
    "S1"=r1/R1*((m12/T2)+((z2*R2)/(T2*r2)) ),
    "p1" <- m12/(m12+z2*R1/r1),
    "lambda" <- r2/R2)
  
  out=data.frame(param=c("S1","p1","lambda"),
                 Estimate=unlist(par_ls))
  
  cat("\n\n Estimates \n")
  print(out,digits=3,row.names=F)
  return(out)
  
}

per2_surph_ests <- function(cell_vals_in,w_table=TRUE){
  
  # SURPH FORMULATION
  R0<-sum(cell_vals_in)
  r0<-sum(cell_vals_in[c("n.11","n.01","n.10")])
  R1<-sum(cell_vals_in[c("n.11","n.10")])
  r1<-cell_vals_in["n.11"]
  m1=sum(cell_vals_in[c("n.11","n.10")])
  m2=sum(cell_vals_in[c("n.01")])
  z1=sum(cell_vals_in[c("n.01")])
  T1=m1+z1
  
  A0<-r0/R0
  A1<-r1/R1
  B1=m1/T1
  B2=1
  
  T1-m1
  # correct lambda
  # correct lambda Skalski 1998 w/ indices shifted down
  (mylam <- r1/R1)
  # correct P1
  (myp1 <- B1/(B1+((1-B1)/A1)))
  # correct P1 Skalski 1998 w/ indices shifted down
  m1/(m1+z1*R1/r1)
  # correct S1
  (mys1 <- A0*(B1 + ((1-B1)/A1)))
  
  
  R0<-sum(cell_vals_in)
  R1<-sum(cell_vals_in[c("n.11","n.10")])
  
  r0<-sum(cell_vals_in[c("n.11","n.01","n.10")])
  r1<-cell_vals_in["n.11"]
  
  m1=sum(cell_vals_in[c("n.11","n.10")])
  m2=sum(cell_vals_in[c("n.01")])
  z1=sum(cell_vals_in[c("n.01")])
  T1=m1+z1
  
  # intermediate
  A0<-r0/R0
  A1<-r1/R1
  B1=m1/T1
  B2=1
  
  # correct P1
  myp1 <- B1/(B1+((1-B1)/A1))
  # correct S1
  mys1 <- A0*(B1 + ((1-B1)/A1))
  # lambda point estimate
  mylam <- r1/R1
  
  
  marr=matrix("",nrow=7,ncol=7)
  marr[,1]=c("i  ","","1  ","2  ","","m[j]","z[j]")
  marr[,2]=c("R[i]","",R0,R1,"","","")
  marr[,4]=c("j=1","",m1,"","",m1,z1)
  marr[,5]=c("j=2","",m2,"","",m2,"")
  marr[,7]=c("r[i]","",r0,r1,"","","")
  
  rownames(marr)=NULL
  colnames(marr)=NULL
  
  tmp <- as.data.frame(marr)
  names(tmp)=rep("",7)
  # cat("m-array table")
  # print(tmp,row.names = F,quote = F)
  # correct P1 Skalski 1998 w/ indices shifted down
  
  par_ls <- list(
    "p1" <- myp1,
    "s1"=mys1,
    "lambda" <- mylam)
  
  
  p1t1 <- (myp1*(1-myp1))^2
  p1t2 <- (1/r1)-(1/R1)+(1/m1)+(1/z1)
  p1SE <- sqrt(p1t1*p1t2)
  
  
  s1t1 <- ((1/r0)-(1/R0))
  s1t2 <- ((1-myp1)^2)*((1/r1)-(1/R1))
  s1t3 <- myp1*(1-myp1)*(((1-A1)^2)/(A1*T1))
  s1SE <- sqrt((mys1^2)*(s1t1+s1t2+s1t3))
  
  # covariance
  cov_s1_p1 <- -1*(mys1*myp1*(1-myp1)^2)*(((1/r1)-(1/R1))+(1-(r1/R1))*(1/z1))
  # print(cov_s1_p1)
  
  lambdaSE <- sqrt((mylam^2)*((1/r1)-(1/R1)))
  
  
  if(!w_table){
    vec=c(mys1,s1SE,myp1,p1SE,mylam,lambdaSE)
    names(vec)=c("S1","s1SE","p1","p1SE","lambda","lambdaSE")
    return(vec)
  }
  
  out=data.frame(param=c("S1","p1","lambda"),
                 Estimate=c(mys1,myp1,mylam),
                 SE=c(s1SE,p1SE,lambdaSE))
  
  # cat("\n\n Estimates \n")
  # print(out,digits=3,row.names=F)
  
  return(out)
  
}



per2_surph_ests_MOD <- function(cell_vals_in,w_table=F){
  
  # SURPH FORMULATION
  R0<-sum(cell_vals_in)
  r0<-sum(cell_vals_in[c("n.11","n.01","n.10")])
  R1<-sum(cell_vals_in[c("n.11","n.10")])
  r1<-cell_vals_in["n.11"]
  m1=sum(cell_vals_in[c("n.11","n.10")])
  m2=sum(cell_vals_in[c("n.01")])
  z1=sum(cell_vals_in[c("n.01")])
  T1=m1+z1
  
  A0<-r0/R0
  A1<-r1/R1
  B1=m1/T1
  B2=1
  
  T1-m1
  # correct lambda
  # correct lambda Skalski 1998 w/ indices shifted down
  (mylam <- r1/R1)
  # correct P1
  (myp1 <- B1/(B1+((1-B1)/A1)))
  # correct P1 Skalski 1998 w/ indices shifted down
  m1/(m1+z1*R1/r1)
  # correct S1
  (mys1 <- A0*(B1 + ((1-B1)/A1)))
  
  
  R0<-sum(cell_vals_in)
  R1<-sum(cell_vals_in[c("n.11","n.10")])
  
  r0<-sum(cell_vals_in[c("n.11","n.01","n.10")])
  r1<-cell_vals_in["n.11"]
  
  m1=sum(cell_vals_in[c("n.11","n.10")])
  m2=sum(cell_vals_in[c("n.01")])
  z1=sum(cell_vals_in[c("n.01")])
  T1=m1+z1
  
  # intermediate
  A0<-r0/R0
  A1<-r1/R1
  B1=m1/T1
  B2=1
  
  # correct P1
  myp1 <- B1/(B1+((1-B1)/A1))
  # correct S1
  mys1 <- A0*(B1 + ((1-B1)/A1))
  # lambda point estimate
  mylam <- r1/R1
  
  
  marr=matrix("",nrow=7,ncol=7)
  marr[,1]=c("i  ","","1  ","2  ","","m[j]","z[j]")
  marr[,2]=c("R[i]","",R0,R1,"","","")
  marr[,4]=c("j=1","",m1,"","",m1,z1)
  marr[,5]=c("j=2","",m2,"","",m2,"")
  marr[,7]=c("r[i]","",r0,r1,"","","")
  
  rownames(marr)=NULL
  colnames(marr)=NULL
  
  tmp <- as.data.frame(marr)
  names(tmp)=rep("",7)
  # cat("m-array table")
  # print(tmp,row.names = F,quote = F)
  # correct P1 Skalski 1998 w/ indices shifted down
  
  par_ls <- list(
    "p1" <- myp1,
    "s1"=mys1,
    "lambda" <- mylam)
  
  
  p1t1 <- (myp1*(1-myp1))^2
  p1t2 <- (1/r1)-(1/R1)+(1/m1)+(1/z1)
  p1SE <- sqrt(p1t1*p1t2)
  
  
  s1t1 <- ((1/r0)-(1/R0))
  s1t2 <- ((1-myp1)^2)*((1/r1)-(1/R1))
  s1t3 <- myp1*(1-myp1)*(((1-A1)^2)/(A1*T1))
  s1SE <- sqrt((mys1^2)*(s1t1+s1t2+s1t3))
  
  # covariance
  cov_s1_p1 <- -1*(mys1*myp1*(1-myp1)^2)*(((1/r1)-(1/R1))+(1-(r1/R1))*(1/z1))
  # print(cov_s1_p1)
  
  lambdaSE <- sqrt((mylam^2)*((1/r1)-(1/R1)))
  
  
  if(!w_table){
    vec=c(mys1,s1SE,myp1,p1SE,mylam,lambdaSE)
    names(vec)=c("S1","s1SE","p1","p1SE","lambda","lambdaSE")
    return(vec)
  }
  
  out=data.frame(param=c("S1","p1","lambda"),
                 Estimate=c(mys1,myp1,mylam),
                 SE=c(s1SE,p1SE,lambdaSE))
  
  # cat("\n\n Estimates \n")
  # print(out,digits=3,row.names=F)
  
  return(out)
  
}
