perm<-function(n,c) {
  if (c==1) {
    return(n)
  } else {
    return(n*perm(n-1,c-1))
  }
}

for (i in 1:30) {
  print(c(i,round((1-perm(365,i)/(365^i)),4)),quote=F)
}

for (i in 1:300) {
  print(c(i,round((1-(364/365)^i),4)),quote=F)
}

