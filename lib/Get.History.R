#---Best History of Player---#
#F,SF,QF,R16,R32,R64,R128

Get.Histroy<-function(Player,Time,n_history){
  library(lubridate)
  library(dplyr)
  Matches = Match[Match$Date<Time,]
  Matches$Round = as.factor(Matches$Round)
  Matches$Level = as.factor(Matches$Level)
  Matches = Matches%>%filter(Winner.Name == Player | Loser.Name == Player)%>%arrange(Round,desc(Date))
  n_history = min(n_history,nrow(Matches))
  History = Matches[1:n_history,c(1:6,9,10,22,23,34)]
  return(History)
}




