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
    tags$div(rHandsontableOutput("table1")),

    br(),
      hr(),
      h6("Referencences:"),
  p("R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL
  https://www.R-project.org/."),
      p(" Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version
  1.0.0. https://CRAN.R-project.org/package=shiny"),
    p("Oliver Gjoneski (2014). rflot: [R] wrapper for flot JS plotting library. R package version 0.1.5."),
      p(" Jonathan Owen (2016). rhandsontable: Interface to the 'Handsontable.js' Library. R package version 0.3.4. https://CRAN.R-project.org/package=rhandsontable")
    )
)
