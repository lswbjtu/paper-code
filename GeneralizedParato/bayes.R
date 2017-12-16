### this code is for bayesian method

#source('BasicFunction.R')
library(HI)
library(MCMCpack)
bayes<-function(R){
  k1=k2=h1=h2=0.001 
  M=1000 ##for ARMS size
  output_arms<-matrix(NA,M,16)
  colnames(output_arms)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                           "beta.AV","beta.MSE","beta.AL","beta.CP",
                           "ST.AV","ST.MSE","ST.AL","ST.CP",
                           "HT.AV","HT.MSE","HT.AL","HT.CP")
  
  paraFuc<-function(BETA){BETA^(n+k2-1)*exp(-sum(log(1+BETA*x))-h2*BETA)}
  indFunc<-function(BETA){BETA>0&BETA<2}
  n<-length(R)
  x<-censoring(c(alpha,beta),R)
  ARMS<-matrix(NA,M,4)
  for (i in 1:sim){
    ARMS[,2]<-arms(rnorm(1),paraFuc,indFunc,M)
    for (k in 1:M){
      ARMS[k,1]<-rgamma(1,n+k1,sum((R+1)*log(1+ARMS[k,2]*x))+h1)
      ARMS[k,3]<-ST(c(ARMS[k,1],ARMS[k,2]))
      ARMS[k,4]<-HT(c(ARMS[k,1],ARMS[k,2]))
    }
    output_arms[i,1]<-mean(ARMS[,1])
    output_arms[i,2]<-sum((ARMS[,1]-alpha)^2)/M
    output_arms[i,3]<-diff(quantile(ARMS[,1],c(0.025,0.975)))
    output_arms[i,4]<-(alpha>quantile(ARMS[,1],0.025))&(alpha<quantile(ARMS[,1],0.975))
    output_arms[i,5]<-mean(ARMS[,2])
    output_arms[i,6]<-sum((ARMS[,2]-beta)^2)/M
    output_arms[i,7]<-diff(quantile(ARMS[,2],c(0.025,0.975)))
    output_arms[i,8]<-(beta>quantile(ARMS[,2],0.025))&(beta<quantile(ARMS[,2],0.975)) 
    output_arms[i,9]<-mean(ARMS[,3])
    output_arms[i,10]<-sum((ARMS[,3]-ST(c(alpha,beta)))^2)/M
    output_arms[i,11]<-diff(quantile(ARMS[,3],c(0.025,0.975)))
    output_arms[i,12]<-(ST(c(alpha,beta))>quantile(ARMS[,3],0.025))&(ST(c(alpha,beta))<quantile(ARMS[,3],0.975))
    output_arms[i,13]<-mean(ARMS[,4])
    output_arms[i,14]<-sum((ARMS[,4]-HT(c(alpha,beta)))^2)/M
    output_arms[i,15]<-diff(quantile(ARMS[,4],c(0.025,0.975)))
    output_arms[i,16]<-(HT(c(alpha,beta))>quantile(ARMS[,4],0.025))&(HT(c(alpha,beta))<quantile(ARMS[,4],0.975))
  }
  output_arms = data.frame(output_arms)
  bayesResult<-apply(output_arms[output_arms$beta.MSE<1,], 2, mean)
  return(bayesResult)
}