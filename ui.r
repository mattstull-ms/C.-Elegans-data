library(shiny)
library(rhandsontable)
shinyUI(navbarPage("QC App",
                   tabPanel("Build",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Model Options:"),
                                br(),
                                selectInput("Model_select", label = h4("Model Type"),
                                             choices = list("Linear" = 1, "Ridge" = 2, "Lasso" = 3,"Elastic Net"=4, "K-Nearest Neighbors" = 5, "Support Vector Machine" = 6, "Multivariate Adaptive Regression Splines" = 7, "Parallel Random Forest" = 8, "Tree Models from Genetic Algorithms" = 9, "Regularized Random Forest" =10, "Quantile Random Forest" = 11, "eXtreme Gradient Boosting" = 12), selected = 1
                                ),
                           
                                br(),
                                h4("Validation Parameters"),
                                numericInput("numfolds", 
                                             label = h5("Number of folds"), 
                                             value = 5, min = 0),
                                numericInput("Repeats",
                                             label = h5("Number of Repeats"),
                                             value = 1, min = 0),
                                actionButton("Build", label = "Build my model")
                                ),
                              mainPanel(
                                p("This modeling and prediction app uses the caret package to train a regression model using repeated k-fold cross validation. It was designed to predict downstream sequencing outputs from upstream QC data."),
                                p("The orignal modeling data can be found", tags$a(href= "https://github.com/mattstull-ms/C.-Elegans-data", "here.")),
                                p("However, you can use whatever type of data you like, so long as you submit independent variables to the downstream portion, and dependent variables to the upstream."),
                                p("Begin by uploading a .csv file of the upstream QC data below. This must have a header row consisting of a set of independent variables."),
                                fileInput("x", label = h4("Upstream data")),
                                rHandsontableOutput("xtable"),
                                br(),
                                p("Next, import a .csv file of the downstream quality metrics you would like your model to predict (i.e. dependent variables). e.g. MAPQ, Read Counts, Clusters, etc."),
                                fileInput("y", label = h4("Downstream data")),
                                rHandsontableOutput("ytable"),
                                br(),
                                p("Select options for your model using the sidebar to your left. Once you are satisfied with your choices, click 'Build my model'. Depending on the size of your data and the number of validation parameters, this may take a couple of minutes."),
                                br()

                                
                                
                              )
                            )),
                   tabPanel("Verify",
                            tabsetPanel(
                             
                              tabPanel("K-fold Validation",tags$div(id = "validation1")),
                             tabPanel("Variable Importance",tags$div(id = "variableimportance1")),
                             tabPanel("Diagnostics", tags$div(id="Plot_1"))
                            
                            )
                   ),            
                   tabPanel("Predict",
                            tabsetPanel(
                              tabPanel("Reactive Table",  
                                       sidebarPanel(width =4,
                                                    fileInput("NewData", label=h4("New data")),
                                                    actionButton("predict_button", label = "Predict"),
                                                    br(),
                                                    uiOutput("update_predict_button_placeholder")
                                       ),
                                       mainPanel(
                                         p("If you are satisfied with how your model is performing, upload some new QC data for which you would like predicted sequencing results and click 'Predict'."),
                                          p("You will then have an interactive table of your data, and another with the predicted outcome. Try editing your data and clicking 'Update' to see how a different value will affect the prediction. Or, for a large project, check out how a change in a data point might affect the 'Summary' tab."),
                                         rHandsontableOutput("xnew"),
                                         br(),
                                         rHandsontableOutput("WTF"),
                                         verbatimTextOutput("vcount")
                                       )),
                              tabPanel("Prediction Summary", 
                                       rHandsontableOutput("xpredictionsummary"),
                                       br(),
                                       rHandsontableOutput("ypredictionsummary")
                              )
                              
                            )
                            
                   )
                   
)
)