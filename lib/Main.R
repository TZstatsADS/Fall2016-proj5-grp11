#Main
source("Monte Carlo Tennis.R")
source("Tournament.R")
source("Get.History.R")

#Example: Simulate Single Match
#"Roger Federer" vs  "Rafael Nadal" on 2015.1.1 on Hard Surface
#Simualte 10000 times
Simulate.Matches = HeadtoHead("2015-01-01","Roger Federer","Rafael Nadal","Hard",10000)
#it will take about one minute
Fit.Truncated.Normal(Simulate.Matches) #Gives out mean and sd of simulated results



#Example: Simulate Tournament (16 players)
Names = unique(Match$Winner.Name)
Name_List = Names[1:16] #As an example 
a = sample(1:16,16)
Draws = Name_List[a] #Set a draw, it can be random or actual 

#Lets simulate 1000 times
Champions = character(1000) #Record champion name of each simualted tournament 
for(i in 1:1000){
  print(i)
  Champions[i] =  Tournament(Name_List,Draws,"2015-01-01","Hard")$Champion
}
table(Champions)



#Example: Get most recent tournametns of a player
Player = "Rafael Nadal"
Get.Histroy(Player,"2015-01-01",10)
