library(shiny)
library(rflot)
library(rhandsontable)
shinyUI(fluidPage(
  
  tags$head(tags$script(type = 'text/javascript', src="http://cdn.rawgit.com/flot/flot/master/jquery.flot.symbol.js")),
  titlePanel("iQC Report"), 
  p("Upload your .csv file to generate a report. Rename the 'Group' column, either in the csv or in the table generated below, to evaluate each of your experimental groups."),
  fluidRow(
    column(6,tags$div(height =200),fileInput("File_1", label = "File")),
    column(6, tags$div(height = 200),actionButton("Go", label = "See my report"))
    
  ),
  
  
    flotOutput("graph1"),
  br(),
 
    tags$div(id = "clickp"),
    br(),
    uiOutput("StatHeader"),
    tags$div(rHandsontableOutput("Stat_table")),
  br(),
    uiOutput("table1header"),
    tags$div(rHandsontableOutput("table1"))

  
))
