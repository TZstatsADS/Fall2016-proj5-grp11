##################
setwd("~/Desktop/哥大/fall 2016/5243 ads/project5/Data & Code")
######data loading
load("Match.RData")
#source('radarchart.R')
load('upset.summary.Rdata')
source('Main.R')
source('Ability.R')
source('Get.History.R')
source('Get.Matches.R')
source('Main.R')
source('Markov.Algo.R')
source('Monte Carlo Tennis.R')
source('New.Tournament.R')
source('Surface.R')
source('Tournament.R')
source('Truncated Normal Distribution.R')
source('dynamic histogram.R')
source('radarchart_new.R')
source('stats_1.R')
source('Tournament.RShiny.R')

library(ggplot2)
library(plotly)
library(stringr)
library(anim.plots)
library(formattable)


Match_dl<-Match
Match_dl <- Match_dl[,-c(3,7,13:20,26:32),drop=FALSE]

TOP25<-read.csv('TOP25.csv')
TOP25_n<-TOP25$name
Names = unique(Match$Winner.Name)
Name_List = Names[1:16]
Name_List2=Names[1:25]
a = sample(1:16,16)
Draws = Name_List[a]
A = read.csv("TOP25.csv",stringsAsFactors = FALSE)  
B= read.csv("TOP25.csv",stringsAsFactors = FALSE)  
Name_List_de=B[1:16,2] 

#######Server
server <- function(input, output,session) {
 
#################intro#####################################################  
   output$ATP<-renderImage({return(list(
     src = "ATP2.png",
     filetype = "image/png",
     alt = "This is a ATP!"
   ))},deleteFile = FALSE)
   
   output$Rank<-renderImage({return(list(
     src = "Rank.png",
     filetype = "image/png",
     alt = "This is rank"
   ))},deleteFile = FALSE)
#################stats######################################################
#    output$stats_1<-renderImage({return(list(
#      src = "4pic.jpg",
#      filetype = "image/jpeg",
#      alt = "Stats Results 1"
#    ))},deleteFile = FALSE)
   
   output$stats_2<-renderPlot({
     plot.serve.win('Hard')
   })
   output$stats_3<-renderPlot({
     plot.serve.win2('Clay')
   })
   
   output$stats_4<-renderPlot({
     plot.serve.win3('Grass')
   })
   
   output$stats_5<-renderPlot({
     as.numeric(Upset.Summary$Num.Games)
     
     drawcolor = c("light blue" ,"pink", "light green", "orange", "light yellow")
     
     ggplot(Upset.Summary,aes(x = Year, y = Upset.Ratio)) + 
       geom_point(aes(size =Num.Upset,color=factor(Year)))+
       guides(colour = FALSE)+
       scale_size_continuous(range=c(10,15))+ 
       scale_y_continuous(limits = c(0,0.5))+
       theme_bw() + 
       ggtitle("Upset Ratio") + 
       labs(x = "Year", 
            y = "Upset Portion") + 
       theme(panel.grid.minor = element_blank()) 
   })
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
  radarplot(input$surface,input$a)
})


output$b_radar<-renderPlot({
  radarplot(input$surface,input$b)
})

output$a_history<-renderTable({Get.Histroy(input$a,input$b,input$date,10)})



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
#   hth_2<-c()
#   hth_2[1]<-percent(hth[1])
  hth[2]<-round(hth[2],3)
  hth[1]<-round(hth[1],4)
  hthdf<-data.frame(
    Name = c("Prob of Player A win", 
             "Sd"),
    Value = c( paste(100*hth[1],"%") ,as.character(hth[2]))
                           
    , 
    stringsAsFactors=FALSE)})



output$hthmean <- renderTable(hthdf())

hthhist_1<-reactive({
  Simulate.Matches<-HeadtoHead(input$date,input$a,input$b,input$surface,input$slider)
  A = as.data.frame(Simulate.Matches)
  qplot(Simulate.Matches,data=A,geom = "histogram",fill = ..count..) + theme_grey(base_size = 18) 
# hthhist_1<-ggplot() + aes(Simulate.Matches)+ geom_histogram(binwidth=0.03, colour="black", fill="blue")
})

output$hthhist<- renderPlot({
  hthhist_1()})


################tournament########################################################


name_16<-eventReactive(input$button2, {
  input$button2
  name_16<-input$name_16
})

surface_t<-eventReactive(input$button2, {
  input$button2
  surface_t<-input$surface_t
  
})

date_t<-eventReactive(input$button2, {
  input$button2
  date_t<-input$date_t
  
})


Champions_t<-reactive({
  b = sample(1:16,16)
  draws_16 = input$name_16[b]
  Champions_t_2<- New.Tournament(name_16(),draws_16,date_t(),surface_t())
  Champions_t_2<-as.data.frame(Champions_t_2)
  Champions_t_2$Champion<-as.character(Champions_t_2$Champion)
  Champions_t_2$Final<-as.character(Champions_t_2$Final)
  Champions_t_2$Semi<-as.character(Champions_t_2$Semi)
  Champions_t_2$Quarter<-as.character(Champions_t_2$Quarter)
  Champions_t_2[2:8,4]<-"-"
  Champions_t_2[3:8,3]<-"-"
  Champions_t_2[5:8,2]<-"-"
Champions_t<-Champions_t_2
  return(Champions_t)
})

#output$hist_t<-renderPlot(dyna_hist_2(Name_List,input$name_16,"2013-08-30",input$surface_t))
#output$pie_t<-renderTable(n_t[2])
output$pie_t<-renderTable(Champions_t())

# dyna<-reactive({
#  if (input$surface_t == "Hard"){
#     A = read.csv("TOP25.csv",stringsAsFactors = FALSE)  
#      Name_List_h=A[1:16,2]  
#      Champions = data.frame(Name=Name_List_h,Count = c(rep(0,16))) 
#      for(i in 1:length(Hard.Result)){  
#        print(i)  
#         temp = Hard.Result[[i]]$Champion  
#         index = grep(temp,Name_List)  
#         Champions[index,2] = Champions[index,2]+1  
#        # barplot(Champions$Count,names.arg= Champions$Name,ylab ="Champion Count",xlab = "Players")  
#         Sys.sleep(0.1)  
#       } 
#      ggplot(data=Champions, aes(x=Name, y=Count,fill=Name)) + geom_bar(stat= "identity")+coord_flip() 
#  }
#   
#   })

# Champions_tp<-reactive({
#   draws_16 = input$name_16[b]
#   Champions_t_2<- New.Tournament(input$name_16,draws_16,"2013-08-30",input$surface_t)
#   return(Champions_t_2)
# })

dyna<-reactive({
  c = sample(1:16,16)
  draws_d = input$name_16[c]
  dyna_1<-New.Tournament.Shiny(name_16(),draws_d,surface_t())
  ggplot(data=dyna_1, aes(x=Name, y=Count,fill=Name)) + geom_bar(stat= "identity")+theme(text=element_text(size = 19),axis.text.x=element_text(angle=30, hjust=1))
  
})
output$hist_t<-renderPlot(

dyna()
  
)

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
