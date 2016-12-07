source("Get.Matches.R")
load("Match.RData")
Player.Ability<-function(Surf,Player,Time="2016-01-01"){
 M = Get.Matches(Player,Match) 
 Match.Win = Match%>%dplyr::filter(Winner.Name == Name & Surface == Surf)%>%dplyr::mutate(Ace = Winner.Num.Ace/Winner.Serve,
                                                                 First.In = Winner.First.In/Winner.Serve,
                                                                 First.Won = Winner.First.Won/Winner.First.In,
                                                                 Second.Won = Winner.Second.Won/(Winner.Serve-Winner.First.In-Winner.Double.Default),
                                                                 Bpsaved = Loser.BPSave.Ratio,
                                                                 Receive.Won = (Loser.Serve-Loser.Second.Won - Loser.First.Won)/Loser.Serve,
                                                                 Double.Default = Winner.Double.Default/Winner.Serve)%>%dplyr::select(Date,Tounament_Name,Ace,First.In,First.Won,Second.Won,Bpsaved,Receive.Won,Double.Default)
 Match.Lose = Match%>%dplyr::filter(Loser.Name == Name & Surface == Surf)%>%dplyr::mutate(Ace = Loser.Num.Ace/Loser.Serve,
                                                                                    First.In = Loser.First.In/Loser.Serve,
                                                                                    First.Won = Loser.First.Won/Loser.First.In,
                                                                                    Second.Won = Loser.Second.Won/(Loser.Serve-Loser.First.In-Loser.Double.Default),
                                                                                    Bpsaved = Loser.BPSave.Ratio,
                                                                                    Receive.Won = (Winner.Serve-Winner.Second.Won - Winner.First.Won)/Winner.Serve,
                                                                                    Double.Default = Loser.Double.Default/Loser.Serve)%>%dplyr::select(Date,Tounament_Name,Ace,First.In,First.Won,Second.Won,Bpsaved,Receive.Won,Double.Default)
 Player.Match = rbind(Match.Win,Match.Lose)
 Player.Match = Player.Match[Player.Match$Date<Time,]
 Player.Match = Player.Match%>%arrange(Date)
 Ability = Player.Match%>%mutate(Serve = Ace + First.In, Attack = First.Won + Second.Won, Defend = Bpsaved + Receive.Won, Antipressure = Bpsaved + Double.Default)%>%
   select(Serve,Attack,Defend,Antipressure)
 Ability = na.omit(Ability)
 scale<-function(x){
   return(mean(5*(x-min(x))/(max(x)-min(x))))
 }
 Ability = apply(Ability,2,scale)
 return(Ability)
}

# Player = Name = "Novak Djokovic"
# Surf = "Hard"
# Player.Ability("Hard","Novak Djokovic")

