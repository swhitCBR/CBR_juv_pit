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
  

  bias_adj_A1 <- (r1+1)/(R1+1)
  bias_adjS <- A0*(B1 + ((1-B1)/bias_adj_A1))
  
  print(mys1)
  print(bias_adjS)
  
  print(mys1-bias_adjS)
  
  
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



lik_fn <- function(pars,nvec){
  
  lo_s1 <- pars[1]
  lo_p1 <- pars[2]
  lo_lambda <- pars[3]
  
  s1 <- plogis(lo_s1)
  p1 <- plogis(lo_p1)
  lambda <- plogis(lo_lambda)
  
  p11<-s1*p1*lambda
  p10<-s1*p1*(1-lambda)
  p01<-s1*(1-p1)*lambda
  p00<-1-s1+s1*(1-p1)*(1-lambda)
  
  nll <- -1*dmultinom(
    x=nvec,
    prob=c(
      p11,
      p10,
      p01,
      p00),log = T)
  
  return(nll)}


# C:/repos/CBR_juv_pit/other/TMB/


TMB_CJS_wrap_logit <- function(){
  
  # compile("C:/repos/CBR_juv_pit/other/TMB/cjs_2per_logit.cpp") # model as of 9-10-2020
  dyn.load( dynlib("C:/repos/CBR_juv_pit/other/TMB/cjs_2per_logit")) 
  TMB:::getUserDLL() # TELLS you which .dll is loaded
  
  # # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  # OBJ = MakeADFun( data=TMB_data,
  #                  parameters=list(
  #                    "p1"=ann_ests[c("p1")],
  #                    "s1"=ann_ests[c("s1")],
  #                    "lambda"=ann_ests[c("lambda")]),
  #                  DLL="cjs_2per",
  #                  random = c(),
  #                  silent = T,hessian = T)
  
  # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  OBJ = MakeADFun( data=TMB_data,
                   parameters=list(
                     "P_lo"=log(0.5/(1-0.5)),
                     "S_lo"=log(0.9/(1-0.9)),
                     "lambda_lo"=log(0.1/(1-0.1))),
                   DLL="cjs_2per_logit",
                   random = c(),
                   silent = T,hessian = T)
  
  # CHECKING THE JOINT LIKELIHOOD AT INITIAL VALUES
  OBJ$report()$nll
  # OBJ <<- OBJ
  
  # Optimizing
  OPT = nlminb( start=OBJ$par,
                objective=OBJ$fn,
                gradient=OBJ$gr) 
  
  SD = sdreport(OBJ,skip.delta.method = F)#,bias.correct = T)#,getReportCovariance = F) # standard errors
  
  cov_mat <- SD$cov.fixed
  hess=OBJ$he(x = OPT$par)
  
  gr=OBJ$gr()
  SEcalc <- sqrt(diag(solve(OBJ$he(OPT$par))))
  
  # prof_ls <- lapply(1:3,function(x) tmbprofile(OBJ,x,adaptive = TRUE,trace=0))
  # par(mfrow=c(3,1))
  # plot(prof_ls[[1]])
  # plot(prof_ls[[2]])
  # plot(prof_ls[[3]])
  # lapply(prof_ls,plot)
  out=data.frame(summary(SD),"SEcalc"=SEcalc)
  
  
  # return(out)
  
  conf_df <- data.frame(par=names(OPT$par),Est=OPT$par
                        # ,
                        #SE=out[,"Std. Error"],
                        # do.call(rbind,lapply(1:3,function(x) 
                          # confint(prof_ls[[x]],level=0.95)))
                        )
  
  dyn.unload( dynlib("cjs_2per_logit"))
  
  out <- list(
    "SEcalc"=SEcalc,
    "OPT"=OPT,
    "out"=out,
    # "gr"=OBJ$gr(),
    "hess"=hess,
    "conf_df"=conf_df,
    "cov_mat"=cov_mat)
  return(out)
  
}


TMB_CJS_wrap_logS_logit_plam <- function(){
  
  # compile("C:/repos/CBR_juv_pit/other/TMB/cjs_2per_logS_logit_plam.cpp") # model as of 9-10-2020
  dyn.load( dynlib("C:/repos/CBR_juv_pit/other/TMB/cjs_2per_logS_logit_plam")) 
  TMB:::getUserDLL() # TELLS you which .dll is loaded
  
  # # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  # OBJ = MakeADFun( data=TMB_data,
  #                  parameters=list(
  #                    "p1"=ann_ests[c("p1")],
  #                    "s1"=ann_ests[c("s1")],
  #                    "lambda"=ann_ests[c("lambda")]),
  #                  DLL="cjs_2per",
  #                  random = c(),
  #                  silent = T,hessian = T)
  
  # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  OBJ = MakeADFun( data=TMB_data,
                   parameters=list(
                     "P_lo"=log(0.5/(1-0.5)),
                     "S_lo"=log(0.9),
                     "lambda_lo"=log(0.1/(1-0.1))),
                   DLL="cjs_2per_logS_logit_plam",
                   random = c(),
                   silent = T,hessian = T)
  
  # CHECKING THE JOINT LIKELIHOOD AT INITIAL VALUES
  OBJ$report()$nll
  # OBJ <<- OBJ
  
  # Optimizing
  OPT = nlminb( start=OBJ$par,
                objective=OBJ$fn,
                gradient=OBJ$gr) 
  
  SD = sdreport(OBJ,skip.delta.method = F)#,bias.correct = T)#,getReportCovariance = F) # standard errors
  
  cov_mat <- SD$cov.fixed
  hess=OBJ$he(x = OPT$par)
  
  gr=OBJ$gr()
  SEcalc <- sqrt(diag(solve(OBJ$he(OPT$par))))
  
  # prof_ls <- lapply(1:3,function(x) tmbprofile(OBJ,x,adaptive = TRUE,trace=0))
  # par(mfrow=c(3,1))
  # plot(prof_ls[[1]])
  # plot(prof_ls[[2]])
  # plot(prof_ls[[3]])
  # lapply(prof_ls,plot)
  out=data.frame(summary(SD),"SEcalc"=SEcalc)
  
  
  # return(out)
  
  conf_df <- data.frame(par=names(OPT$par),Est=OPT$par
                        # ,
                        #SE=out[,"Std. Error"],
                        # do.call(rbind,lapply(1:3,function(x) 
                        # confint(prof_ls[[x]],level=0.95)))
  )
  
  dyn.unload( dynlib("cjs_2per_logS_logit_plam"))
  
  out <- list(
    "SEcalc"=SEcalc,
    "OPT"=OPT,
    "out"=out,
    # "gr"=OBJ$gr(),
    "hess"=hess,
    "conf_df"=conf_df,
    "cov_mat"=cov_mat)
  return(out)
  
}


TMB_CJS_wrap <- function(){
  
  compile("C:/repos/CBR_juv_pit/other/TMB/cjs_2per.cpp") # model as of 9-10-2020
  dyn.load( dynlib("C:/repos/CBR_juv_pit/other/TMB/cjs_2per")) 
  TMB:::getUserDLL() # TELLS you which .dll is loaded
  
  # # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  # OBJ = MakeADFun( data=TMB_data,
  #                  parameters=list(
  #                    "p1"=ann_ests[c("p1")],
  #                    "s1"=ann_ests[c("s1")],
  #                    "lambda"=ann_ests[c("lambda")]),
  #                  DLL="cjs_2per",
  #                  random = c(),
  #                  silent = T,hessian = T)
  
  # # # FOR SOME REASON THIS WORKS BUT NOT THE ABOVE CODE
  OBJ = MakeADFun( data=TMB_data,
                   parameters=list(
                     "p1"=0.5,
                     "s1"=0.9,
                     "lambda"=0.1),
                   DLL="cjs_2per",
                   random = c(),
                   silent = T,hessian = T)
  
  # CHECKING THE JOINT LIKELIHOOD AT INITIAL VALUES
  OBJ$report()$nll
  # OBJ <<- OBJ
  
  # Optimizing
  OPT = nlminb( start=OBJ$par,
                objective=OBJ$fn,
                gradient=OBJ$gr) 
  
  SD = sdreport(OBJ,skip.delta.method = F)#,bias.correct = T)#,getReportCovariance = F) # standard errors
  
  cov_mat <- SD$cov.fixed
  hess=OBJ$he(x = OPT$par)
  
  gr=OBJ$gr()
  SEcalc <- sqrt(diag(solve(OBJ$he(OPT$par))))
  
  prof_ls <- lapply(1:3,function(x) tmbprofile(OBJ,x,adaptive = TRUE,trace=0))
  # par(mfrow=c(3,1))
  # plot(prof_ls[[1]])
  # plot(prof_ls[[2]])
  # plot(prof_ls[[3]])
  # lapply(prof_ls,plot)
  out=data.frame(summary(SD),"SEcalc"=SEcalc)
  
  
  # return(out)
  
  conf_df <- data.frame(par=names(OPT$par),Est=OPT$par,#SE=out[,"Std. Error"],
                        do.call(rbind,lapply(1:3,function(x) confint(prof_ls[[x]],level=0.95))))
  
  dyn.unload( dynlib("cjs_2per"))
  
  out <- list(
    "SEcalc"=SEcalc,
    "OPT"=OPT,
    "out"=out,
    # "gr"=OBJ$gr(),
    "hess"=hess,
    "conf_df"=conf_df,
    "cov_mat"=cov_mat)
  return(out)
  
}
  