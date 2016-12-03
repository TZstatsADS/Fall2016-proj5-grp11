Get.Matches<-function(Name,Match){
  setwd("~/Desktop/ATP project/tennis_atp-master")
  library(dplyr)
  Match= Match%>%filter(Winner.Name == Name | Loser.Name == Name)
  return(Match)
}

Get.Serve.Win<-function(Match,Name){
  Match.Win = Match%>%filter(Winner.Name == Name)%>%dplyr::mutate(Serve.Win =(Winner.First.Won + Winner.Second.Won)/Winner.Serve)%>%dplyr::select(Serve.Win)
  Match.Lose = Match%>%filter(Loser.Name == Name)%>%dplyr::mutate(Serve.Win = (Loser.First.Won + Loser.Second.Won)/Loser.Serve )%>%dplyr::select(Serve.Win)
  Serve.Win = rbind(Match.Win,Match.Lose)
  return(Serve.Win)
}

Get.Receive.Win<-function(Match,Name){
   Match.Win = Match%>%filter(Winner.Name == Name)%>%dplyr::mutate(Receive.Win = (Loser.Serve - Loser.First.Won - Loser.Second.Won)/Loser.Serve)%>%dplyr::select(Receive.Win)
   Match.Lose = Match%>%filter(Loser.Name == Name)%>%dplyr::mutate(Receive.Win = (Winner.Serve - Winner.First.Won - Winner.Second.Won)/Winner.Serve)%>%dplyr::select(Receive.Win)
   Receive.Win = rbind(Match.Win,Match.Lose)
   return(Receive.Win)
}
