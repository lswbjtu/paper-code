### This code is for  mle estimate

#source('BasicFunction.R')

library(numDeriv)
mle<-function(R){
  simResult<-matrix(NA,sim,16)
  colnames(simResult)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
  mseResult<-matrix(NA,1,16)
  colnames(mseResult)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                        "beta.AV","beta.MSE","beta.AL","beta.CP",
                        "ST.AV","ST.MSE","ST.AL","ST.CP",
                        "HT.AV","HT.MSE","HT.AL","HT.CP")
  for (i in 1:sim){
    n<-length(R)
    data<-censoring(c(alpha,beta),R)
    res1<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),
                x=data,R=R,hessian=T,control=list(trace=F,maxit=1000))
    simResult[i,1]<-res1$par[1]
    simResult[i,3]<-2*qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])
    simResult[i,4]<-(res1$par[1]-qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])<alpha
                     )&(alpha<res1$par[1]+qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1]))
    
    simResult[i,5]<-res1$par[2]
    simResult[i,7]<-2*qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2])
    simResult[i,8]<-(res1$par[2]-qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2])<beta
                     )&(beta<res1$par[2]+qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2]))
  
    simResult[i,9]<-ST(res1$par)
    Varhat1<-(t(grad(ST,res1$par))%*%solve(res1$hessian)%*%grad(ST,res1$par))[1,1]
    simResult[i,11]<-2*qnorm((1+conf.level)/2)*sqrt(Varhat1)
    simResult[i,12]<-(ST(res1$par)-0.5*simResult[i,11]<ST(c(alpha,beta)))&(ST(c(alpha,beta))<ST(res1$par)+0.5*simResult[i,11])
    
    simResult[i,13]<-HT(res1$par)
    Varhat2<-(t(grad(HT, res1$par))%*%solve(res1$hessian)%*%grad(HT, res1$par))[1,1]
    simResult[i,15]<-2*qnorm((1+conf.level)/2)*sqrt(Varhat2)
    simResult[i,16]<-(HT(res1$par)-0.5*simResult[i,15]<HT(c(alpha,beta)))&(HT(c(alpha,beta))<HT(res1$par)+simResult[i,15])
  }
  mseResult[,1]<-mean(simResult[,1])
  mseResult[,2]<-sum((simResult[,1]-mean(simResult[,1]))^2)/sim
  mseResult[,3]<-mean(simResult[,3])
  mseResult[,4]<-mean(simResult[,4])
  mseResult[,5]<-mean(simResult[,5])
  mseResult[,6]<-sum((simResult[,5]-mean(simResult[,5]))^2)/sim
  mseResult[,7]<-mean(simResult[,7])
  mseResult[,8]<-mean(simResult[,8])
  mseResult[,9]<-mean(simResult[,9])
  mseResult[,10]<-sum((simResult[,9]-mean(simResult[,9]))^2)/sim
  mseResult[,11]<-mean(simResult[,11])
  mseResult[,12]<-mean(simResult[,12])
  mseResult[,13]<-mean(simResult[,13])
  mseResult[,14]<-sum((simResult[,13]-mean(simResult[,13]))^2)/sim
  mseResult[,15]<-mean(simResult[,15])
  mseResult[,16]<-mean(simResult[,16])
  return(mseResult)
}
