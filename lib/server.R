##################
setwd("~/Desktop/哥大/fall 2016/5243 ads/project5/Data & Code")
######data loading
load("Match.RData")
source('radarchart.R')
source('Main_test.R')
library(ggplot2)
library(plotly)


Match_dl<-Match
Match_dl <- Match_dl[,-c(3,7,13:20,26:32),drop=FALSE]


#######Server
server <- function(input, output,session) {
  
#################player#####################################################
a_name<-reactive({
  a_name<-input$a
})

b_name<-reactive({
  b_name<-input$b
})

output$a_photo<-renderImage({
  
  
  # When input$n is 3, filename is ./images/image3.jpeg
  filename <- normalizePath(file.path(getwd(),
                                      paste0(input$a,'.png')))
  
  # Return a list containing the filename and alt text
  return(list(src = filename,contentType = "image/png",
       alt = paste("No official ATP image of", input$a)))
  
}, deleteFile = FALSE)


output$b_photo<-renderImage({
  
  
  # When input$n is 3, filename is ./images/image3.jpeg
  filename <- normalizePath(file.path(getwd(),
                                      paste0(input$b,'.png')))
  
  # Return a list containing the filename and alt text
  return(list(src = filename,contentType = "image/png",
              alt = paste("No official ATP image of", input$b)))
  
}, deleteFile = FALSE)


output$a_radar<-renderPlot({
  radarplot(input$a)
})


output$b_radar<-renderPlot({
  radarplot(input$b)
})


surface<-reactive({
  surface<-input$surface
})

slider<-reactive({
  slider<-input$slider
})

#output$hth<-renderDataTable(
  #Simulate.Matches=HeadtoHead(input$date,input$a,input$b,input$surface,input$slider)
  #hth=Fit.Truncated.Normal(Simulate.Matches)
#)


hthdf<-reactive({
  Simulate.Matches<-HeadtoHead(input$date,input$a,input$b,input$surface,input$slider)
  hth<-Fit.Truncated.Normal(Simulate.Matches)
  hthdf<-data.frame(
    Name = c("Prob of Player A win", 
             "Sd"),
    Value = c(as.character(hth[1]),as.character(hth[2]))
                           
    , 
    stringsAsFactors=FALSE)})



output$hthmean <- renderTable(hthdf())

hthhist_1<-reactive({
  Simulate.Matches<-HeadtoHead(input$date,input$a,input$b,input$surface,input$slider)
 hthhist_1<-hist(Simulate.Matches,breaks=50)
# hthhist_1<-ggplot() + aes(Simulate.Matches)+ geom_histogram(binwidth=0.03, colour="black", fill="blue")
})

output$hthhist<- renderPlot({
  hthhist_1})



  
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
