##################
setwd("~/Desktop/哥大/fall 2016/5243 ads/project5/Data & Code")
######data loading
load("Match.RData")
Match_dl<-Match
Match_dl <- Match_dl[,-c(3,7,13:20,26:32),drop=FALSE]


#######Server
server <- function(input, output) {
  
  
  
################data set reference########################################################
  output$table <- DT::renderDataTable(DT::datatable({
    Match_dl
  }, rownames = FALSE))
  
  output$downloadData <- downloadHandler(
    filename = 'file.csv',
    content = function(file) {
      write.csv(Match_dl, file,row.names=F)
    }
  )
}