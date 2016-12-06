dyna_hist<-function(t_n,Name_List,surface){
  library(ggplot2)
  a = sample(1:16,16)
  Draws = Name_List[a]
  Champions = data.frame(Name=Name_List,Count = c(rep(0,16)))
  champion = c(rep(0,16))
  for(i in 1:10){
    print(i)
    Champion =  New.Tournament(Name_List,Draws,"2013-01-01",surface)$Champion
    index = grep(Champion,Name_List)
    Champions[index,2] = Champions[index,2]+1
    barplot(Champions$Count,names.arg= Champions$Name)
    #print(ggplot(Champions,aes(y = Count, x = Name))+geom_bar(stat = "identity"))
    Sys.sleep(0)
  }
  }


# dyna_hist(100,Name_List,"Hard")
# Names = unique(Match$Winner.Name)
# Name_List = Names[1:16]
# a = sample(1:16,16)
# Draws = Name_List[a]
# surface = "Hard"
# t1 = Sys.time()
# Tournament(Name_List,Draws,"2013-08-30","Hard")
# t2 = Sys.time()
# New.Tournament(Name_List,Draws,"2013-08-30","Hard")
# t3 = Sys.time()


load("Hard.Result.RData")
A = read.csv("TOP25.csv",stringsAsFactors = FALSE)
Name_List = A[1:16,2]
Champions = data.frame(Name=Name_List,Count = c(rep(0,16)))
for(i in 1:length(Hard.Result)){
  print(i)
  temp = Hard.Result[[i]]$Champion
  index = grep(temp,Name_List)
  Champions[index,2] = Champions[index,2]+1
  barplot(Champions$Count,names.arg= Champions$Name,ylab ="Champion Count",xlab = "Players")
  Sys.sleep(0.1)
}
