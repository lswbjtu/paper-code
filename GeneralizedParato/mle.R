### This code is for  mle estimate

source('BasicFunction.R')

mleEstimate<-function(R){
  mseResult<-matrix(NA,sim,16)
  colnames(EVandMSE)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
  for (i in 1:sim){
    n<-length(R)
    data<-censoring(c(alpha,beta),R)
    res1<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),
                x=data,R=R,hessian=T,control=list(trace=F,maxit=1000))
    mseResult[i,1]<-res1$par[1]
    mseResult[i,2]<-2*qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])
    mseResult[i,3]<-(res1$par[1]-qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])<alpha
                     )&(alpha<res1$par[1]+qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1]))
    mseResult[i,5]<-res1$par[2]
    mseResult[i,9]<-ST(res1$par)
    mseResult[i,13]<-HT(res1$par)
    
  }
  
}