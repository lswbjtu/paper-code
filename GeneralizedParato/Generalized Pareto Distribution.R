########### Failure rate function ##########
GPDFR<-function(w)
{
  x<-seq(0,w,0.001)
  n<-length(x)
  j<-1
  hx<-numeric(n)
  
  FR<-function(x,theta){ 
    repeat{
      hx[j]<-theta[1]*theta[2]*(1+theta[2]*x[j])^(-1) ##########
      j<-j+1
      if(j>length(x))
        break
    }
    hx
  }
  
  par(mfrow=c(1,1))
  plot(x,FR(x,c(0.5,1)),type="l",xlab="x",ylab = "h(x)", xlim=c(0,w), ylim=c(0,2),col=1,lwd=2)
  lines(x,FR(x,c(0.8,1)),lty=2,xlim=c(0,w+0.1), col=2,lwd=2)
  lines(x,FR(x,c(1,1)),lty=3,xlim=c(0,w+0.1), col=3,lwd=2)
  lines(x,FR(x,c(2,1)),lty=4,xlim=c(0,w+0.1), col=4,lwd=2)
  lines(x,FR(x,c(3,1)),lty=5,xlim=c(0,w+0.1), col=5,lwd=2)
  
  exbeta<-c(expression(paste(alpha,"=0.5,",beta,"=1")),expression(paste(alpha,"=0.8,",beta,"=1")),expression(paste(alpha,"=1,",beta,"=1")),expression(paste(alpha,"=2,",beta,"=1")),expression(paste(alpha,"=3,",beta,"=1")))
  legend("topright", exbeta, lty = c(1, 2,3,4,5),col=c(1,2,3,4,5),lwd=2)
}
GPDFR(4)

####### MLE ##########
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

log.lik1<-function(theta,x,R){
  n<-length(x)
  N<-n+sum(R)
  c0<-rep(N,n)
  c<-prod(c0)
  ln<-log(c)+n*log(theta[1])+n*log(theta[2])-sum((theta[1]*R+theta[1]+1)*log(1+theta[2]*x))
  return(-ln)
}


t<-1.5   ################这个值可以改为其他数字

ST<-function(theta){(1+theta[2]*t)^(-theta[1])}   ########## t0=1.5       
HT<-function(theta){theta[1]*theta[2]*(1+theta[2]*t)^(-1)}   ########## t0=1.5       

R1<-c(20,rep(0,29))  
R2<-c(rep(0,10),rep(2,10),rep(0,10))
R3<-c(rep(0,29),20)
scheme<-rbind(R1,R2,R3)   #### 有效样本大小m相同的删失模式可以合并成矩阵进行循环

alpha<-0.3
beta<-1
sim<-1000

EVandMSE<-matrix(NA,nrow(scheme),8)
colnames(EVandMSE)<-c("alpha.EV","alpha.mse","beta.EV","beta.mse","ST.EV","ST.mse","HT.EV","HT.mse")
rownames(EVandMSE)<-c("R1","R2","R3")

for ( i in 1: nrow(scheme) ) {
  R<-scheme[i,]
  
  output<-matrix(NA,sim,4)
  
  for (j in 1:sim){
    data<-censoring(c(alpha,beta),R)
    res1<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),x=data,R=R,hessian=T,control=list(trace=F,maxit=1000))
    output[j,]<-c(res1$par,ST(res1$par),HT(res1$par))
  }
  
  EV<-apply(output[sim/2:sim,],2,mean)
  Real<-c(alpha,beta,ST(c(alpha,beta)),HT(c(alpha,beta)))
  
  bias1 <- EV-Real
  var1 <- apply(output,2,var) * ((sim-1)/sim)
  mse1 <- bias1^2 + var1
  
  EVandMSE[i,]<-as.vector(rbind(EV,mse1))
}

Real

EVandMSE


###### Confidence interval estimation using ML ##########

scheme<-rbind(R1,R2,R3)   #### 有效样本大小m相同的删失模式可以合并成矩阵进行循环

alpha<-0.3
beta<-1
sim<-1000
conf.level<-0.95

LandC<-matrix(NA,nrow(scheme),8)
colnames(LandC)<-c("alpha.AL","alpha.CP","beta.AL","beta.CP","S(t).AL","S(t).CP","H(t).AL","H(t).CP")
rownames(LandC)<-c("R1","R2","R3")


for ( i in 1: nrow(scheme) ) {
  R<-scheme[i,]
  n<-length(R)
  
  output<-matrix(NA,sim,8)  ###  每个区间的长度和覆盖情况
  
  for (j in 1:sim){
    data<-censoring(c(alpha,beta),R)
    res1<-optim(c(1,1),log.lik1,method="L-BFGS-B",lower=c(0.1,0.1),x=data,R=R,hessian=T,control=list(trace=F,maxit=1000))
    output[j,1]<-2*qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])
    output[j,2]<-(res1$par[1]-qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1])<alpha)&(alpha<res1$par[1]+qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[1,1]))
    output[j,3]<-2*qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2])
    output[j,4]<-(res1$par[2]-qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2])<beta)&(beta<res1$par[2]+qnorm((1+conf.level)/2)*sqrt(solve(res1$hessian)[2,2]))
    library(numDeriv)
    Varhat1<-(t(grad(ST,res1$par))%*%solve(res1$hessian)%*%grad(ST,res1$par))[1,1]
    output[j,5]<-2*qnorm((1+conf.level)/2)*sqrt(Varhat1)
    output[j,6]<-(ST(res1$par)-output[j,5]<ST(c(alpha,beta)))&(ST(c(alpha,beta))<ST(res1$par)+output[j,5])
    Varhat2<-(t(grad(HT, res1$par))%*%solve(res1$hessian)%*%grad(HT, res1$par))[1,1]
    output[j,7]<-2*qnorm((1+conf.level)/2)*sqrt(Varhat2)
    output[j,8]<-(HT(res1$par)-output[j,7]<HT(c(alpha,beta)))&(HT(c(alpha,beta))<HT(res1$par)+output[j,7])
  
  LandC[i,]<-apply(output,2,mean)
  }
}
LandC



################ Boot-p 
alpha<-0.3
beta<-1
conf.level<-0.95
Bboot<-1000  
sim<-500
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

apply(R1[R1$beta.MSE<1,], 2, mean)
apply(R2[R2$beta.MSE<1,], 2, mean)
apply(R3[R3$beta.MSE<1,], 2, mean)


####### Bayes method 
k1=k2=h1=h2=0.001 ################以下是贝叶斯估计方法
sim=1000##20000 for bias and mse
M=1000 ##for ARMS size
output_arms<-matrix(NA,M,16)
colnames(output_arms)<-c("alpha.AV","alpha.MSE","alpha.AL","alpha.CP",
                         "beta.AV","beta.MSE","beta.AL","beta.CP",
                         "ST.AV","ST.MSE","ST.AL","ST.CP",
                         "HT.AV","HT.MSE","HT.AL","HT.CP")
paraFuc<-function(BETA){BETA^(n+k2-1)*exp(-sum(log(1+BETA*x))-h2*BETA)}
indFunc<-function(BETA){BETA>0}
R<-scheme[1,] ## 1,2,3
n<-length(R)
x<-censoring(c(alpha,beta),R)
ARMS<-matrix(NA,M,4)
for (i in 1:sim){
  ARMS[,2]<-arms(rexp(1,beta),paraFuc,indFunc,M)
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
apply(output_arms[output_arms$beta.MSE<1,], 2, mean)  





cat("Calculation took", proc.time()[1], "seconds.\n")

############# Real data analysis ##############
data1<-c(12, 15, 22, 24, 24, 32, 32, 33, 34, 38, 38, 43, 44, 48, 52, 53, 54, 54,55, 56, 57, 58, 58, 59, 60, 60, 60, 60, 61, 62, 63, 65, 65, 67, 68, 70, 70, 72, 73, 75, 76, 76, 81, 83,84, 85, 87, 91, 95, 96, 98, 99, 109, 110, 121, 127, 129, 131, 143, 146, 146, 175, 175, 211, 233,258, 258, 263, 297, 341, 341, 376)

MLE1<-function(x){
  n<-length(x)
  log.lik1<-function(theta,x){       
    ln<-n*log(theta[1])+n*log(theta[2])-theta[2]*sum(1/x)+sum(log(1/x^2))+(theta[1]-1)*sum(log(1-exp(-theta[2]/x)))
    return(-ln)
  }
  res<-optim(c(0.5,0.5), log.lik1,method="L-BFGS-B",lower=c(0.01,0.01), x=x, hessian=T,control=list(trace=F,maxit=1000))
  
  F1<-function(x,theta){1-(1-exp(-theta[2]/x))^theta[1]}   ####cdf of GIED
  K<-ks.test(x,F1,res$par)
  
  canshu1<-res$par
  nlnL<-res$val
  AIC<-2*length(canshu1)+2*nlnL
  BIC<-length(canshu1)*log(n)+2*nlnL
  KS<-K$statistic
  P.value<-K$p.value
  
  
  
  
  plot(F1(sort(x),res$par),(1:n-0.5)/n,col=2,xlab="Fitted",ylab="Empirical")
  abline(0,1,col=4)
  c(canshu1,nlnL,AIC,BIC,KS,P.value)
}

MLE1(data1)

########如果拟合其他双参数分布函数，则需要换掉log.lik1和 F1，如果拟合单参数分布，还需换掉res中初始值。








