#Upset
load("Match.RData")
Champions = NULL
tournaments = unique(Match$Tounament_Name)
Champions<-Match%>%arrange(Date)%>%filter(Round == "F")%>%select(Date,Tounament_Name,Winner.Name)
upset = matrix("0",ncol=4,nrow = dim(Champions)[1])
upset.index = 1

for(i in 1:(dim(Champions)[1]-1)){
  print(i)
  champ = Champions[i,3]
  M = Champions[1,]
  M = M[-1,]
  next.round = 1
  j = i
  while(dim(M)[1]==0){
  M = Match%>%filter(Tounament_Name == Champions[(j+1),2],(Winner.Name==champ | Loser.Name == champ))
  j = j +1
  if( j == dim(Champions)[1]){
    next.round= 0
    print("No games")
    break
  }
  }
  if(next.round==1 & dim(M)[1]<=2 ){
    upset[upset.index,] = c(Champions[i,2],champ,M[1,2],M[dim(M)[1],3])
    upset.index = upset.index+1
  }
}

upset[upset=="0"] = NA
upset = na.omit(upset)
save(upset,file = "upset.RData")
