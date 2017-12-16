### This code is for simulation
source('BasicFunction.R')
source('mle.R')
source('bootstrap.R')
source('bayes.R')

### Set parameters
R1<-c(20,rep(0,29))  
R2<-c(rep(0,10),rep(2,10),rep(0,10))
R3<-c(rep(0,29),20)
scheme<-rbind(R1,R2,R3)   #### 有效样本大小m相同的删失模式可以合并成矩阵进行循环

alpha<-0.3
beta<-1
sim<-100
conf.level<-0.95
t=1.5

########simluation
mle(R = R2)
bootstrap()
bayes(R2)




