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
      
      
      ################################################################################################   time series part           
      
      
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
                 column(6,textInput("a", label = h3("Player A"), value = "Roger Federer"),
                        
                        
                        hr()
                        ),
                 #actionButton("button","Re-run simulation", icon("random")),
                 column(6,textInput("b", label = h3("Player B"), value = "Rafael Nadal"),
                        
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
                 column(6,
                        h3("Head to Head Explorer"),
                        sliderInput('slider', 'Number of simulations', 
                                    min=1, max=10000,
                                    value=500, 
                                    step=500, round=0),
                        br(),
                        radioButtons("surface", "Surface type:",
                                     c("Clay" = "Clay",
                                       "Hard" = "Hard",
                                       "Grass" = "Grass")),
                        dateInput('date',
                                  label = 'Date input: yyyy-mm-dd',
                                  value = Sys.Date())
                        
                         ),column(3, tableOutput("hthmean"))
                
                 
               ),fluidRow(
                 box(width = 12, plotOutput("hthhist"))
                 
               )
               
     )
     
     
     ),
      ################################################################################################  prediction part                 
   # tabItem(tabName = "tournament",),
      ################################################################################################ data set part
    tabItem(tabName = "dataset",
            box(width = 20,
                DT::dataTableOutput("table"),downloadButton('downloadData', 'Download')
            ))
    
  )
)
)


