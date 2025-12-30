
get_mom_mat <- function(count_tab){
  
  input_dat <- count_tab[,match(c("X1.1","X1.0","X0.1","X0.0"),names(count_tab))] %>% replace(is.na(.), 0) 
  # print(input_dat)
  count_tab <- count_tab[,-match(c("X1.1","X1.0","X0.1","X0.0"),names(count_tab))]#,input_dat)
  
  est.mat<-t(apply(input_dat,1,get.mom))
  res_out <- cbind(count_tab,est.mat)
  res_out$no_estimate <- (is.na(res_out$s1) | is.infinite(res_out$s1))
  
  res_out$s1_var <- sapply(1:nrow(res_out),function(x){
    S.var(c(res_out$N[x],res_out$s1[x],res_out$p1[x],res_out$lambda[x]))})
  
  # res_out$s1_var=S.var(res_out$N,res_out$s1,res_out$p1,res_out$lambda)
  res_out}



# calculate MLE's of parameters - these are the Method-of-Moment estimates
get.mom<-function(n.vec)
{
  names(n.vec)<-c("n.11","n.10","n.01","n.00")
  N<-sum(n.vec)
  a1<-sum(n.vec[c("n.11","n.10")])
  b1<-n.vec["n.11"]
  b0<-N-n.vec["n.00"]
  g1<-sum(n.vec[c("n.11","n.01")])
  
  p1.mom<-b1/g1
  s1.mom<-(a1/N)/p1.mom
  lambda.mom<-b1/a1
  
  out<-c(N,a1,b1,b0,g1,s1.mom,p1.mom,lambda.mom)
  names(out)<-c("N","a1","b1","b0","g1","s1","p1","lambda")
  out <- c(n.vec,out)
  
  return(out)
}


# analytical surival estimates for 2 period model
S.var<-function(theta)
{
  # computes approximate variance of survival estimate of 2-site CJS model
  # theta = c(N,S,P,lambda)
  # N = release size
  # S = survival in first reach
  # P = detection probability at first site
  # lambda = joint probability of survival and detection in second reach
  
  N<-theta[1]
  S<-theta[2]
  P<-theta[3]
  lambda<-theta[4]
  
  # not simplified (direct from Delta method)
  var.Sa<-((P+lambda-1)/(N*P*lambda))^2*(N*S*P*lambda*(1-S*P*lambda)) + 
    (1/(N*P))^2*N*S*P*(1-lambda)*(1-S*P*(1-lambda)) + 
    (1/(N*lambda))^2*N*S*(1-P)*lambda*(1-S*(1-P)*lambda) +
    -2*((P+lambda-1)/(N*P*lambda))*(1/(N*P))*N*S*P*lambda*S*P*(1-lambda) + 
    -2*((P+lambda-1)/(N*P*lambda))*(1/(N*lambda))*N*S*P*lambda*S*(1-P)*lambda + 
    -2*(1/(N*P))*(1/(N*lambda))*N*S*P*(1-lambda)*S*(1-P)*lambda
  
  # simplified:
  x1<-P^2 + (1-lambda)^2 - 2*P*(1-lambda) - S*P*lambda*(P^2 - (1-lambda)^2) + lambda*(1-lambda) - S*P*lambda*((1-lambda)^2) + P*(1-P) - S*P*((1-P)^2)*lambda - 2*S*P^2*(1-P)*lambda
  var.Sb<-(S/(N*P*lambda))*x1
  
  #print(c(var.Sa, var.Sb))
  
  var.S<-var.Sb
  
  return(var.S)
}

