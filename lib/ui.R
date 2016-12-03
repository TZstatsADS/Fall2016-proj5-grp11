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
      menuItem("Prediction", tabName = "predict", icon = icon("area-chart")),
      menuItem("Data Reference",tabName = "dataset", icon = icon("table"))
    ),
    div(includeMarkdown("atpinfo.md"), style = "padding:10px")
  ),
  dashboardBody(
    tags$head(tags$script(src = "js/ga.js")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_fixs.css")),
    
      
      ################################################################################################ crime map
      
      
      ################################################################################################   time series part           
      
      
      ################################################################################################   public facility part           
     tabItem(tabName = "players",)
      ################################################################################################  prediction part                 
      
      ################################################################################################ data set part
    tabItem(tabName = "dataset",
            box(width = 20,
                DT::dataTableOutput("table"),downloadButton('downloadData', 'Download')
            ))
    
  )
)



