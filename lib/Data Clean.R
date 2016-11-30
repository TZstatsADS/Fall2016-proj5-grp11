Get.Full.Matches<-function(){
setwd("~/Desktop/ATP project/tennis_atp-master")
library(data.table)
Match_2015<-fread("atp_matches_2015.csv")
Match_2014<-fread("atp_matches_2014.csv")
Match_2013<-fread("atp_matches_2013.csv")
Match_2012<-fread("atp_matches_2012.csv")
Match_2011<-fread("atp_matches_2011.csv")
Match = rbind(Match_2013,Match_2014)
Match = rbind(Match,Match_2015)
Match = rbind(Match,Match_2012)
Match = rbind(Match,Match_2011)
library(dplyr)
library(lubridate)
table(Match$tourney_level)
Cleaned_Match = Match%>%mutate(Date = ymd(tourney_date),
                               Round = round,
                               Level = tourney_level,
                               Surface = surface,
                               Num.Set = best_of,
                               Time = minutes,
                               Winner = winner_id,
                               Winner.Rank = winner_rank,
                               Winner.Name = winner_name,
                               Winner.Age = winner_age,
                               Winner.Height = winner_ht,
                               Winner.Hand = winner_hand,
                               Winner.Serve = w_svpt,
                               Winner.Num.Ace = w_ace,
                               Winner.Double.Default = w_df,
                               Winner.First.In = w_1stIn,
                               Winner.First.Won = w_1stWon,
                               Winner.Second.Won = w_2ndWon,
                               Winner.BPSave.Ratio = w_bpSaved/w_bpFaced,
                               Loser = loser_id,
                               Loser.Rank = loser_rank,
                               Loser.Name = loser_name,
                               Loser.Age = loser_age,
                               Loser.Height = loser_ht,
                               Loser.Hand = loser_hand,
                               Loser.Serve = l_svpt,
                               Loser.Num.Ace = l_ace,
                               Loser.Double.Default = l_df,
                               Loser.First.In = l_1stIn,
                               Loser.First.Won = l_1stWon,
                               Loser.Second.Won = l_2ndWon,
                               Loser.BPSave.Ratio = l_bpSaved/l_bpFaced,
                               Score = score)
Cleaned_Match = Cleaned_Match[,50:82]
Match = Cleaned_Match%>%filter(Level%in%c("C","G","M","F","A"))
return(Match)
}

