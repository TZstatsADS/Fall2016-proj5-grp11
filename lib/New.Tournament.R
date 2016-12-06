New.Tournament<-function(Name_List,Draws,Time,Surface){
  library(truncnorm)
  library(stringr)
  Matches = Match[Match$Date<Time,]
  Field = Get.Surface(Surface,Matches)
  Fea = NULL
  for( i in 1:length(Name_List)){
    Fea[[i]] = Get.Fea(Name_List[i],Matches)
  }
  n_name = length(Name_List) 
  n_draws = length(Draws)
  if(n_name!=16 | n_draws!=16){
    print("Wrong input!")
    break
  }
  Quarter = character(8)
  Semi = character(4)
  Final = character(2)
  Champion = character(1)
  #Quarter
  for(i in 1:8){
    A = grep(Draws[(2*(i-1)+1)],Name_List)
    B = grep(Draws[2*i],Name_List)
    Temp = New.Para(Fea[[A]],Fea[[B]])
    simu.result = Simulate(Temp$new.pa,Temp$new.pb)
    if(simu.result == 1){
      Quarter[i] = Draws[(2*(i-1)+1)]
    }else{
      Quarter[i] = Draws[2*i]
    }
  }
  #Semi
  for(i in 1:4){
    A = grep(Quarter[(2*(i-1)+1)],Name_List)
    B = grep(Quarter[2*i],Name_List)
    Temp = New.Para(Fea[[A]],Fea[[B]])
    simu.result = Simulate(Temp$new.pa,Temp$new.pb)
    if(simu.result == 1){
      Semi[i] = Quarter[(2*(i-1)+1)]
    }else{
      Semi[i] = Quarter[2*i]
    }
  }
  #Final
  for(i in 1:2){
    A = grep(Semi[(2*(i-1)+1)],Name_List)
    B = grep(Semi[2*i],Name_List)
    Temp = New.Para(Fea[[A]],Fea[[B]])
    simu.result = Simulate(Temp$new.pa,Temp$new.pb)
    if(simu.result == 1){
      Final[i] = Semi[(2*(i-1)+1)]
    }else{
      Final[i] = Semi[2*i]
    }
  }
  
  A = grep(Final[1],Name_List)
  B = grep(Final[2],Name_List)
  Temp = New.Para(Fea[[A]],Fea[[B]])
  simu.result = Simulate(Temp$new.pa,Temp$new.pb)
  simu.result = rbinom(1,1,Prob)
  if(simu.result == 1) Champion = Final[1]
  if(simu.result == 0) Champion = Final[2]
  return(list(Quarter=Quarter,Semi=Semi,Final=Final,Champion=Champion))
}


Get.Fea<-function(PlayerA,Matches){
  PlayerA.Match = Get.Matches(PlayerA,Matches)
  Serve.Win.A = Get.Serve.Win(PlayerA.Match,PlayerA)
  Receive.Win.A = Get.Receive.Win(PlayerA.Match,PlayerA)
  Fit.Truncated.Normal<-function(x){
    mu = mean(x)
    sdd = sd(x)
    return(c(mean.tnorm(mu,sdd,0,1),var.tnorm(mu,sdd,0,1)))
  }
  pa = Fit.Truncated.Normal(Serve.Win.A[!is.na(Serve.Win.A)])
  ra = Fit.Truncated.Normal(Receive.Win.A[!is.na(Receive.Win.A)])
  return(list(pa=pa,ra=ra))
}


New.Para<-function(FeaA,FeaB){
  pa = FeaA$pa
  ra = FeaA$ra
  pb = FeaB$pa
  rb = FeaB$ra
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
  if(d>0){
    new.pa[2] = pa[2]*d
    new.pb[2] = pb[2]*d
  }else{
    new.pa[2] = pa[2]
    new.pb[2] = pb[2]
  }
  return(list(new.pa = new.pa,new.pb=new.pb))
}

Simulate<-function(new.pa,new.pb){
  pa.simu = rtruncnorm(n=1,a=0,b=1,mean = new.pa[1],sd=new.pa[2])
  pb.simu = rtruncnorm(n=1,a=0,b=1,mean = new.pb[1],sd=new.pb[2])
  prob = Prob.Match.Win(pa = pa.simu, pb = pb.simu)
  simu.result = rbinom(1,1,prob)
  return(simu.result)
}


# Names = unique(Match$Winner.Name)
# Name_List = Names[1:16]
# a = sample(1:16,16)
# Draws = Name_List[a]
# Surface = "Hard"
# t1 = Sys.time()
# Tournament(Name_List,Draws,"2013-08-30","Hard")
# t2 = Sys.time()
# New.Tournament(Name_List,Draws,"2013-08-30","Hard")
# t3 = Sys.time()