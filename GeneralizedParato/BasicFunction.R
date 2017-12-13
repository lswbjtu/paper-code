### This code is for the distritbuion characteristic

PDF<-function(theta,x){theta[1]*theta[2]*(1+theta[2]*x)^(-theta[1]-1)}
CDF<-function(theta,x){1-(1+theta[2]*x)^(-theta[1])}  ##theta is c(alpha,beta) for GDP
ST<-function(theta){(1+theta[2]*t)^(-theta[1])}         
HT<-function(theta){theta[1]*theta[2]*(1+theta[2]*t)^(-1)}   ###### t is parameter

### generate cencoring data
censoring<-function(theta,R){
  n<-length(R)
  W<-runif(n)
  V<-rep(0,n)
  U<-rep(0,n)
  for (i in 1:n)
  {
    V[i]<-W[i]^(1/(i+sum(R[(n-i+1):n])))
  }
  for (i in 1:n){
    U[i]<-1-prod(V[n:(n-i+1)])
  }
  return(1/theta[2]*(1-U)^(-1/theta[1])-1/theta[2])
}

### Log-likelihood for cencoring data
log.lik1<-function(theta,x,R){
  n<-length(x)
  N<-n+sum(R)
  c0<-rep(N,n)
  c<-prod(c0)
  ln<-log(c)+n*log(theta[1])+n*log(theta[2])-sum((theta[1]*R+theta[1]+1)*log(1+theta[2]*x))
  return(-ln)
}

