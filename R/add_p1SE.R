
#
add_p1SE <- function(subb){
  
  print("Don't trust this function! use get_ests_from_MSS()")
  
  # standard error of p1 parameter
  
  subb$R1=subb$N
  subb$R2=0
  subb$m12=subb$n.10+subb$n.11
  subb$m13=subb$n.01
  subb$m2=subb$m12
  subb$m3=subb$m13
  subb$T2=subb$m2+subb$m3
  subb$T3=subb$m3
  subb$z2=subb$m3
  subb$r1=subb$n.10 +subb$n.11
  
  stopifnot(all(subb$T2==subb$z2+subb$m2))
  
  subb$r2=0
  stopifnot(all(subb$T3==subb$z2+subb$r2))
  
  subb$p1SE=(subb$p1*(1-subb$p1))^2 + ((1/subb$r1)-(1/subb$R1) + (1/subb$m2) + (1/subb$z2))
  return(subb)
}


