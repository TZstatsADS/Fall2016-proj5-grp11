
load('Match')
install.packages("fmsb")
library(fmsb)

colnames(Match)

measure <- c('Winner.Height','Winner.Age',  'Winner.Hand', 'Winner.Num.Ace', 'Winner.BPSave.Ratio')
apply(Match[,measure],2,range, na.rm=T) #see range of measure

#scale.data <- function(a.data){
#  a.data$Winner.Age <- (a.data$Winner.Age-10)/(40-10)
#  a.data$Winner.Height <- (a.data$Winner.Height-160)/(208-160)
#  a.data$Winner.Num.Ace <- a.data$Winner.Num.Ace/53
#  return(a.data)
#  }

radarplot <- function(playername){
  data <- Match[which(Match$Winner.Name == playername),]
  measure <- c('Winner.Height','Winner.Age',  'Winner.Hand', 'Winner.Num.Ace', 'Winner.BPSave.Ratio')
  data <- data[ , measure]
  data$Winner.Hand <- ifelse(data$Winner.Hand == 'R', 1, 0) # R=1,LorU=0
  
  mean <-as.data.frame(t(as.matrix(colMeans(data, na.rm = T))))
  
  #build right format for radarchart 
  a.data <- rbind(c(208,40,1,53,1),c(160,15,0,0,0), mean)
  names(a.data) <- c('Height','Age',  'Hand', 'Num.Ace', 'BPSave.Ratio')

  Radar <- radarchart(a.data , axistype = 2,
                   
                   #custom polygon
                   pcol=rgb(0.2,0.2,0.5,0.6), pfcol=rgb(0.2,0.2,0.5,0.3) , plwd=3,
                   
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", cglwd=1,
                   
                   #custom labels
                   vlcex=0.8,
                   title = playername)
  
 return( c(Radar,mean) )
}

save(radarplot, file = "C:/Users/Mengya/Desktop/Columbia Desk/GR5243/Project5/plotradar.Rdata")

namelist <- unique(Match$Winner.Name);length(namelist)
i<-1
radarplot(namelist[i] )

i <- i+1
           
           

