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
                   ),
                   tabPanel("Citations",
                            h3("References"),
                            p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version
                              1.0.0. https://CRAN.R-project.org/package=shiny"),
                            p("Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton
                              Kenkel, the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang, Can Candan and Tyler Hunt. (2016).
                              caret: Classification and Regression Training. R package version 6.0-73. https://CRAN.R-project.org/package=caret"),
                            p("Jonathan Owen (2016). rhandsontable: Interface to the 'Handsontable.js' Library. R package version 0.3.4.
                              https://CRAN.R-project.org/package=rhandsontable"),
                            p(" Hui Zou and Trevor Hastie (2012). elasticnet: Elastic-Net for Sparse Estimation and Sparse PCA. R package version 1.1.
                              https://CRAN.R-project.org/package=elasticnet"),
                            p("Alexandros Karatzoglou, Alex Smola, Kurt Hornik, Achim Zeileis (2004). kernlab - An S4 Package for Kernel Methods in R. Journal of
                              Statistical Software 11(9), 1-20. URL http://www.jstatsoft.org/v11/i09/"),
                            p("David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2017). e1071: Misc Functions of the Department of
                              Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.6-8. https://CRAN.R-project.org/package=e1071"),
                            p("Stephen Milborrow. Derived from mda:mars by Trevor Hastie and Rob Tibshirani. Uses Alan Miller's Fortran utilities with Thomas Lumley's
                              leaps wrapper. (2017). earth: Multivariate Adaptive Regression Splines. R package version 4.4.9. https://CRAN.R-project.org/package=earth"),
                            p("Revolution Analytics and Steve Weston (2015). doMC: Foreach Parallel Adaptor for 'parallel'. R package version 1.3.4.
                              https://CRAN.R-project.org/package=doMC"),
                            p("Thomas Grubinger, Achim Zeileis, Karl-Peter Pfeiffer (2014). evtree: Evolutionary Learning of Globally Optimal Classification and
                              Regression Trees in R. Journal of Statistical Software, 61(1), 1-29. URL http://www.jstatsoft.org/v61/i01/."),
                            p("H. Deng(2013). Guided Random Forest in the RRF Package. arXiv:1306.0237."),
                              
                              p("H. Deng and G. Runger (2013). Gene Selection with Guided Regularized Random Forest. Pattern Recognition 46(12): 3483-3489."),
                              
                              p("H. Deng and G. Runger (2012). Feature Selection via Regularized Trees. The 2012 International Joint Conference on Neural Networks (IJCNN)."),
                            p("Nicolai Meinshausen (2016). quantregForest: Quantile Regression Forests. R package version 1.3-5.
                              https://CRAN.R-project.org/package=quantregForest"),
                            p("  Tianqi Chen, Tong He, Michael Benesty, Vadim Khotilovich and Yuan Tang (2017). xgboost: Extreme Gradient Boosting. R package version 0.6-4.
                              https://CRAN.R-project.org/package=xgboost")
                            )
                   
)
)
