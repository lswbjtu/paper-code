### This code is for bootstrap method

#source('BasicFunction.R')

bootstrap<-function(){
  Bboot<-1000 
  output<-matrix(NA,3,16)
  boot.p<-matrix(NA,Bboot,4)
  colnames(output)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                      "beta.AV","beta.MSE","beta.AL","beta.CP",
                      "ST.AV","ST.MSE","ST.AL","ST.CP",
                      "HT.AV","HT.MSE","HT.AL","HT.CP")
  rownames(output)<-c("R1","R2","R3")
  output_R1 = matrix(NA,sim,16)
  output_R2 = matrix(NA,sim,16)
  output_R3 = matrix(NA,sim,16)
  colnames(output_R1)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
  colnames(output_R2)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
  colnames(output_R3)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
  for (k in 1:sim){
    for (j in 1:nrow(scheme)){
      R<-scheme[j,]
      n<-length(R)
      data<-censoring(c(alpha,beta),R)
      res1<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),x=data,R=R,hessian=T,control=list(trace=F,maxit=1000))
      boost.res<-res1
      for (i in 1:Bboot){
        bootsample<-censoring(boost.res$par,R)
        boot.res<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),x=bootsample,R=R,hessian=T,control=list(trace=F,maxit=1000))
        boot.p[i,1]<-boot.res$par[1]
        boot.p[i,2]<-boot.res$par[2]
        boot.p[i,3]<-ST(boot.res$par)
        boot.p[i,4]<-HT(boot.res$par)
      }
      output[j,1]<-mean(boot.p[,1])
      output[j,2]<-sum((boot.p[,1]-alpha)^2)/Bboot
      output[j,3]<-diff(quantile(boot.p[,1],c(0.025,0.975)))
      output[j,4]<-(alpha>quantile(boot.p[,1],0.025))&(alpha<quantile(boot.p[,1],0.975))
      
      output[j,5]<-mean(boot.p[,2])
      output[j,6]<-sum((boot.p[,2]-beta)^2)/Bboot
      output[j,7]<-diff(quantile(boot.p[,2],c(0.025,0.975)))
      output[j,8]<-(beta>quantile(boot.p[,2],0.025))&(beta<quantile(boot.p[,2],0.975))
      
      output[j,9]<-mean(boot.p[,3])
      output[j,10]<-sum((boot.p[,3]-ST(c(alpha,beta)))^2)/Bboot
      output[j,11]<-diff(quantile(boot.p[,3],c(0.025,0.975)))
      output[j,12]<-(ST(c(alpha,beta))>quantile(boot.p[,3],0.025))&(ST(c(alpha,beta))<quantile(boot.p[,3],0.975))
      
      output[j,13]<-mean(boot.p[,4])
      output[j,14]<-sum((boot.p[,4]-HT(c(alpha,beta)))^2)/Bboot
      output[j,15]<-diff(quantile(boot.p[,4],c(0.025,0.975)))
      output[j,16]<-(HT(c(alpha,beta))>quantile(boot.p[,4],0.025))&(HT(c(alpha,beta))<quantile(boot.p[,4],0.975))
    }
    output_R1[k,]<-output[1,]
    output_R2[k,]<-output[2,]
    output_R3[k,]<-output[3,]
  }
  R1<-data.frame(output_R1)
  R2<-data.frame(output_R2)
  R3<-data.frame(output_R3)
  
  bootResult<-matrix(data=NA,3,16)
  colnames(bootResult)<- c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                          "beta.AV","beta.MSE","beta.AL","beta.CP",
                          "ST.AV","ST.MSE","ST.AL","ST.CP",
                          "HT.AV","HT.MSE","HT.AL","HT.CP")
  rownames(bootResult)<-c('R1','R2','R3')
  bootResult<-data.frame(bootResult)
  
  bootResult[1,]<-apply(R1[R1$beta.MSE<1,], 2, mean)
  bootResult[2,]<-apply(R2[R2$beta.MSE<1,], 2, mean)
  bootResult[3,]<-apply(R3[R3$beta.MSE<1,], 2, mean)
  
  return(bootResult)
}
  