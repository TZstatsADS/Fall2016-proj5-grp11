source("Monte Carlo Tennis.R")
Tournament<-function(Name_List,Draws,Time,Surface){ 
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
  Prob = HeadtoHead(ymd(Time),Draws[(2*(i-1)+1)],Draws[2*i],Surface,1)
  simu.result = rbinom(1,1,Prob)
  if(simu.result == 1){
    Quarter[i] = Draws[(2*(i-1)+1)]
  }else{
    Quarter[i] = Draws[2*i]
  }
}

#Semi
  for(i in 1:4){
    Prob = HeadtoHead(ymd(Time),Quarter[(2*(i-1)+1)],Quarter[2*i],Surface,1)
    simu.result = rbinom(1,1,Prob)
    if(simu.result == 1){
      Semi[i] = Quarter[(2*(i-1)+1)]
    }else{
      Semi[i] = Quarter[2*i]
    }
  }
#Final
  for(i in 1:2){
    Prob = HeadtoHead(ymd(Time),Semi[(2*(i-1)+1)],Semi[2*i],Surface,1)
    simu.result = rbinom(1,1,Prob)
    if(simu.result == 1){
      Final[i] = Semi[(2*(i-1)+1)]
    }else{
      Final[i] = Semi[2*i]
    }
  }
#Final
  Prob = HeadtoHead(ymd(Time),Final[1],Final[2],Surface,1)
  simu.result = rbinom(1,1,Prob)
  if(simu.result == 1) Champion = Final[1]
  if(simu.result == 0) Champion = Final[2]
  return(list(Quarter=Quarter,Semi=Semi,Final=Final,Champion=Champion))
}


# Names = unique(Match$Winner.Name)
# Name_List = Names[1:16]
# a = sample(1:16,16)
# Draws = Name_List[a]
# Surface = "Hard"
# Tournament(Name_List,Draws,"2013-08-30","Hard")
# 
# Champions = character(1000)
# for(i in 1:1000){
#   print(i)
#   Champions[i] =  Tournament(Name_List,Draws,"2013-08-30","Hard")$Champion
# }
