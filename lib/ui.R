#########################################################################################################load library
library("shiny")
library("shinydashboard")
library("highcharter")
library("dplyr")
library("viridisLite")
library("markdown")
library("quantmod")
library("tidyr")
library("treemap")
library("forecast")
library("DT")
library("shiny")
library("leaflet")
library("plotly")
library("wordcloud2")
library('scatterD3')
library('png')
library('imager')
#source("renderInputs.R")


#########################################################################################################clear environment
rm(list = ls())



#########################################################################################################main page begin
dashboardPage(
  skin = "blue",
  dashboardHeader(title = "ATP Tennis Player Analysis", disable = FALSE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("map-marker")),
      menuItem("Stats", tabName = "stats", icon = icon("line-chart")),
      menuItem("Players",tabName = "players", icon = icon("list-alt")),
      menuItem("Tournament", tabName = "tournament", icon = icon("area-chart")),
      menuItem("Data Reference",tabName = "dataset", icon = icon("table"))
    ),
    div(includeMarkdown("atpinfo.md"), style = "padding:10px")
  ),
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),tabItems(
    
      
      ################################################################################################ crime map
      tabItem(tabName = "intro",fluidPage(theme="simplex.min.css",
                                          tags$style(type="text/css",
                                                     "label {font-size: 30px;}",
                                                     ".recalculating {opacity: 1.0;}"
                                          ),
        fluidRow(column(9,imageOutput("ATP"))),
        fluidRow(column(9,imageOutput("Rank"))),
        #tags$img(src="https://www.logomoose.com/wp-content/uploads/2016/03/tennis_logo.png"), 
       # tags$img(src = "https://media.giphy.com/media/5qdVDVUoPOeI0/giphy.gif"),
        tags$h1("Analysis on Tennis Players and Game Simulation"),
        tags$h3("We used a stochastic Markov chain model to obtain the probability density function (pdf) for a player to win a match in tennis.Based on these Gaussian distributed input variables. And we also used Monte Carlo simulations to determine the probability density functions for each of the players to win a match. ",
          br(),
          tags$a(href="https://www.degruyter.com/downloadpdf/j/jqas.2009.5.3/jqas.2009.5.3.1169/jqas.2009.5.3.1169.xml", "Essay Reference"),br(),
          tags$a(href="http://www.atpworldtour.com/en/rankings/singles?rankDate=2015-12-28", "~2015 Ranking")
         
         )
        
        
      )),
      
      ################################################################################################   time series part           
      
#       tabItem(tabName = 'stats',fluidPage(fluidRow(
#         column(12,imageOutput("stats_1", height = "600px"),
#                
#                
#                hr()
#         )))),
      
      
      tabItem(tabName = 'stats',fluidPage(fluidRow(
        column(12,plotOutput("stats_2", height = "600px"),
               
               
               hr(),column(12,plotOutput("stats_3", height = "600px"),
                           
                           
                           hr(),column(12,plotOutput("stats_4", height = "600px"),
                                       hr(),column(12,plotOutput("stats_5", height = "600px")
                           
        ))      )
             )
            ))
        ),
      ################################################################################################   public facility part           
     tabItem(tabName = "players", 
     
     # Define UI for application that plots random distributions
     fluidPage(theme="simplex.min.css",
               tags$style(type="text/css",
                          "label {font-size: 12px;}",
                          ".recalculating {opacity: 1.0;}"
               ),
               
               # Application title
               tags$h2("Head to Head: Simulating game results with Monte Carlo Methods"),
               p("A prediction of head to head",
                 tags$a(href="http://www.atpworldtour.com", "tennis games"),
                 "based on",
                 tags$a(href="https://github.com/JeffSackmann/tennis_atp", "ATP data"),
                 "to demonstrate the use of Shiny's new grid options."),
               hr(),
               
               #fluidRow(
                # column(6, tags$h3("Player A")),
                 #column(6, tags$h3("Player B"))
               #),
               fluidRow(
                 column(6,textInput("a", label = h3("Player A"), value = "Andy Murray"),
                        
                        
                        hr()
                        ),
                 #actionButton("button","Re-run simulation", icon("random")),
                 column(6,textInput("b", label = h3("Player B"), value = "Novak Djokovic"),
                        
                        hr())
               ),
               fluidRow(
                 column(6,
                        imageOutput("a_photo", height = "400px")
                 ),
                 column(6,
                        imageOutput("b_photo", height = "400px")
                 )
               ),
               fluidRow(
                 box(width = 6, plotOutput("a_radar")),
                 box(width = 6, plotOutput("b_radar"))
               ),
               fluidRow(
                 box(width = 12, tableOutput("a_history"),align="center")
                 
               ),
               fluidRow(
                 column(6,
                        h1("Head to Head Explorer"),
                        div(style="font-size: 700%;",sliderInput('slider', 'Number of simulations', 
                                    min=1, max=10000,
                                    value=500, 
                                    step=500, round=0)),
                        br(),
                        radioButtons("surface", "Surface type:",
                                     c("Clay" = "Clay",
                                       "Hard" = "Hard",
                                       "Grass" = "Grass")),
                        dateInput('date',
                                  label = 'Date input: yyyy-mm-dd',
                                  value = Sys.Date())
                        
                         ),column(3, div(tableOutput("hthmean"),style = "font-size: 225%; width: 175%"))
                
                 
               ),fluidRow(
                 box(width = 12, plotOutput("hthhist"))
                 
               )
               
     )
     
     
     ),
      ################################################################################################  prediction part                 
    tabItem(tabName = "tournament",
   
        fluidPage(fluidRow(
          column(3,
                 h3("Head to Head Explorer"),
                 selectInput("surface_t", label = "Surface",
                             choices = c("Hard", 
                                         "Clay", 
                                         "Grass")
                 ),
                 br(),
              selectizeInput(
                     'name_16', '16 players', c("Andy Murray" = "Andy Murray",
                                                "Novak Djokovic" = "Novak Djokovic",
                                                "Milos Raonic" = "Milos Raonic","Stanislas Wawrinka"="Stanislas Wawrinka",
                                                "Kei Nishikori"="Kei Nishikori","Marin Cilic"="Marin Cilic",
                                                "Gael Monfils"="Gael Monfils","Dominic Thiem"="Dominic Thiem","Rafael Nadal"="Rafael Nadal",
                                                "Tomas Berdych"="Tomas Berdych","David Goffin"="David Goffin","Jo Wilfried Tsonga"="Jo Wilfried Tsonga","Nick Kyrgios"="Nick Kyrgios","Roberto Bautista Agut"="Roberto Bautista Agut","Lucas Pouille"="Lucas Pouille" ,"Roger Federer"="Roger Federer","Grigor Dimitrov"="Grigor Dimitrov","Richard Gasquet"="Richard Gasquet",
                                                "John Isner"="John Isner","Ivo Karlovic"="Ivo Karlovic","David Ferrer"="David Ferrer","Pablo Cuevas"="Pablo Cuevas","Jack Sock"="Jack Sock","Alexander Zverev"="Alexander Zverev","Gilles Simon"="Gilles Simon"), multiple = TRUE,options = list(maxItems = 16),selected=Name_List_de
                   ),

#  checkboxGroupInput("name_16", label = "Player Name",
#                     choices = c("Leonardo Mayer" = "Leonardo Mayer",
#                                 "Horacio Zeballos" = "Horacio Zeballos",
#                                   "Marcel Granollers" = "Marcel Granollers","Guillermo Garcia Lopez"="Guillermo Garcia Lopez","Robin Haase"="Robin Haase","Daniel Brands"="Daniel Brands","Albert Montanes"="Albert Montanes","Victor Hanescu"="Victor Hanescu","Dominic Thiem"="Dominic Thiem",
#                      "Jan Hajek"="Jan Hajek","Daniel Gimeno Traver"="Daniel Gimeno Traver","Andreas Haider Maurer"="Andreas Haider Maurer","Fernando Verdasco"="Fernando Verdasco","Juan Monaco"="Juan Monaco","Xavier Malisse"="Xavier Malisse" ,"Yen Hsun Lu"="Yen Hsun Lu","Roberto Bautista Agut"="Roberto Bautista Agut","Carlos Berlocq"="Carlos Berlocq",
#      "Evgeny Donskoy"="Evgeny Donskoy","Nicolas Mahut"="Nicolas Mahut","Andrey Kuznetsov"="Andrey Kuznetsov","Jan Hernych"="Jan Hernych","Michael Llodra"="Michael Llodra","Jeremy Chardy"="Jeremy Chardy","Marius Copil"="Marius Copil"),
#                     selected = Draws
#            ),

dateInput('date_t',
          label = 'Date input: yyyy-mm-dd',
          value = Sys.Date()),
actionButton("button2", "Update", 
         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               
          ),column(6,tableOutput("pie_t"))
#column(3,imageOutput("hist_t"))

          
        ),fluidRow(
          box(width = 12,imageOutput("hist_t") ))
    )),
      ################################################################################################ data set part
    tabItem(tabName = "dataset",
            box(width = 20,
                DT::dataTableOutput("table"),downloadButton('downloadData', 'Download')
            ))
    
  )
)
)


