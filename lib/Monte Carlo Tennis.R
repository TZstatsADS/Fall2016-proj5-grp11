#Monte Carlo Tennis: A stochastic Markov Chain Model
source("Get.Matches.R")
source("Surface.R")
source("Markov.Algo.R")
source("Truncated Normal Distribution.R")
load("Match.RData")

HeadtoHead<-function(Time,PlayerA,PlayerB,Surface,n_simulation){
library(lubridate)
Time = ymd("2016-01-01")
Matches = Match[Match$Date<Time,]
PlayerA.Match = Get.Matches(PlayerA,Matches)
PlayerB.Match = Get.Matches(PlayerB, Matches)
#PlayerA
Serve.Win.A = Get.Serve.Win(PlayerA.Match,PlayerA)
Receive.Win.A = Get.Receive.Win(PlayerA.Match,PlayerA)
Fit.Truncated.Normal<-function(x){
 mu = mean(x)
 sdd = sd(x)
 return(c(mean.tnorm(mu,sdd,0,1),var.tnorm(mu,sdd,0,1)))
}

pa = Fit.Truncated.Normal(Serve.Win.A[!is.na(Serve.Win.A)])
ra = Fit.Truncated.Normal(Receive.Win.A[!is.na(Receive.Win.A)])
#PlayerB
Serve.Win.B = Get.Serve.Win(PlayerB.Match,PlayerB)
Receive.Win.B = Get.Receive.Win(PlayerB.Match,PlayerB)
pb = Fit.Truncated.Normal(Serve.Win.B[!is.na(Serve.Win.B)])
rb = Fit.Truncated.Normal(Receive.Win.B[!is.na(Receive.Win.B)])
#Simulate 5000 Games between two players
library(msm)
Field = Get.Surface(Surface,Matches)
Prob.Match = numeric(n_simulation)
A = matrix(0,nrow=n_simulation,ncol=3)
i = 1
uar = ra[1] - Field$Return.mean
ubr = rb[1] - Field$Return.mean
ta = as.numeric((pa[1]-ubr)/pa[1])
new.pa = new.pb = numeric(2)
new.pa[1] = pa[1] - ubr
new.pb[1] = pb[1] - uar
if(new.pa[1]>new.pb[1]){
  d = -(new.pa[1] - new.pb[1]-0.15)/(1.5*(pa[2]+pb[2]))
}else{
  d = -(new.pb[1] - new.pa[1]-0.15)/(1.5*(pa[2]+pb[2]))
}
new.pa[2] = pa[2]*d
new.pb[2] = pb[2]*d
while(i<=n_simulation){
pa.simu = rtruncnorm(n=1,a=0,b=1,mean = new.pa[1],sd=new.pa[2])
pb.simu = rtruncnorm(n=1,a=0,b=1,mean = new.pb[1],sd=new.pb[2])
Prob.Match[i] = Prob.Match.Win(pa = pa.simu, pb = pb.simu)
A[i,] = c(pa.simu,pb.simu,Prob.Match[i])
i = i+1
}
return(Prob.Match)
}










