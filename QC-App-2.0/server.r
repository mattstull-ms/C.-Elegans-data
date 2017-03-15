library(shiny)
library(caret)
library(rhandsontable)
library(elasticnet)
library(kernlab)
library(e1071)
library(earth)
library(doMC)
library(evtree)
library(RRF)
library(quantregForest)
library(xgboost)
shinyServer(function(input,output){
  #registerDoMC(cores = 4)
  #Seems like this ^ breaks the app in the cloud
  q<-reactiveValues(b = 0)
  #After build button clicked....
  observeEvent(input$Build,{
    showModal(modalDialog(
      title = "Calculating...",
      "This may take some time. Please be patient.",
      easyClose = TRUE,
      footer = NULL
    ))
    q$b<-q$b+1
    if(q$b == 1){
      #import upstream qc data
      filex<-input$x
      if (is.null(filex)){
        return(NULL)
      }
      xdata<-read.csv(filex$datapath)
      xcolcnt<-length(xdata[1,])
      xcolumn_names<-colnames(xdata)
      #import downstream data
      filey<-input$y
      if(is.null(filey)){
        return(NULL)
      }
      ylist<-read.csv(filey$datapath)
      ydata<-as.data.frame(ylist)
      ycolcnt<-length(ydata[1,])
      ycolumn_names<-colnames(ydata)
      
      #return it as an rhandsontable
      ydata_table<-rhandsontable(ydata, width = 750, height = 500, readOnly = TRUE)%>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      xdata_table<-rhandsontable(xdata, width = 750, height = 500, readOnly = TRUE)%>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      output$xtable<-renderRHandsontable(hot_heatmap(xdata_table, color_scale = c("#0066ff", "#ffff00")))
      output$ytable<-renderRHandsontable(hot_heatmap(ydata_table, color_scale = c("#0066ff", "#ffff00")))
      
      #set repeated k fold cv with slider inputs
      fitControl<-trainControl(method = "repeatedcv", repeats = input$Repeats, number = input$numfolds)
      
      #compute model with full data set
      if(input$Model_select == 1){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "lm", trControl = fitControl))
        }
      }
      else if(input$Model_select == 2){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "ridge", trControl = fitControl))
        }
      }
      else if(input$Model_select == 3){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "lasso", trControl = fitControl))
        }
      }
      else if(input$Model_select == 4){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "enet", trControl = fitControl))
        }
      }
      else if(input$Model_select == 5){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "knn", trControl = fitControl))
        }
      }
      else if(input$Model_select == 7){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "gcvEarth", trControl = fitControl))
        }
      }
      else if(input$Model_select == 8){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "parRF", trControl = fitControl, importance = TRUE))
        }
      }
      else if(input$Model_select == 9){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "evtree", trControl = fitControl, importance = TRUE))
        }
      }
      else if(input$Model_select == 10){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "RRF", trControl = fitControl, importance = TRUE))
        }
      }
      else if(input$Model_select == 11){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "qrf", trControl = fitControl, importance = TRUE))
        }
      }
      else if(input$Model_select == 12){
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "xgbTree", trControl = fitControl, importance = TRUE))
        }
      }
      else{
        for(i in 1:ycolcnt){
          y<-ydata[,i]
          assign(paste0("newmodel",i),train(xdata,y, method = "svmLinear2", trControl = fitControl))
        }
      }
      
      #Instruct user to move on after model built
      showModal(modalDialog(
        title = "Congratulations!",
        "You have built a model. Click on the 'Verify' tab to explore how well it behaves. Feel free to make changes to your data or model and click on the build button again. Otherwise, start making predictions!",
        easyClose = TRUE,
        footer = NULL
      ))
      
      #Print model summary for each dependent variable
      #output$kfoldvar1<-renderPrint(newmodel1)
      for(i in 1:ycolcnt){
        local({
          x<-i
          insertUI(selector = "#validation1",
                   ui = tags$div(id = paste0("Validationdiv",x),tags$h5(ycolumn_names[x]), tableOutput(paste("kfoldvar",x,sep=""))))
          output[[paste("kfoldvar",x,sep="")]]<-renderTable(print(eval(parse(text = paste("newmodel",x,sep=""))), selectCol = TRUE))
        })
      }
      
      #print variable importance for each dependent variable
      for(i in 1:ycolcnt){
        local({
          x<-i
          insertUI(selector = "#variableimportance1",
                   ui=tags$div(id = paste0("variableimportancediv",x),tags$h5(ycolumn_names[x]), verbatimTextOutput(paste("variable_importance",x,sep=""))))
          output[[paste("variable_importance",x,sep="")]]<-renderPrint(varImp(eval(parse(text = paste("newmodel",x,sep="")))))
        })
      }
      
      
      #Uses a for loop to iteratively generate panels with diagnostic plots for the number of dependent variables  
      for(i in 1:ycolcnt){local({
        x<-i
        insertUI(selector= "#Plot_1", 
                 where = "beforeEnd",
                 ui=tags$div(id = paste0("plotdiv",x),tags$h5(ycolumn_names[x]), plotOutput(paste("Diagnostic_",x, sep = ""), height = "800px"))
        )
        
      })}
      for(t in 1:ycolcnt){local({
        j<-t
        #calculate theoretical quantile
        q<-qqnorm(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j])
        #Calculate influence (h)
        X<-as.matrix(xdata)
        H<-X%*%solve(t(X)%*%X)%*%t(X)
        h<-diag(H)
        #calculate components of cooks distance
        #e=error
        e<-predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j]
        #s = RMSE
        s<-eval(parse(text = paste("newmodel",j,"$results$RMSE",sep="")))
        #p=no. independent vars
        p<-length(xdata[1,])
        #Cooks Distance
        D<-e*e/(s*s*p)*(h/((1-h)*(1-h)))
        #find values greater than 1
        #labs<-NULL
        #for(i in 1:length(D)){
         # if(D[i] >=1){
          #  append(labs,i)
         # }
       # }
        output[[paste("Diagnostic_",j,sep="")]]<-renderPlot({
          par(mfrow=c(2,2))
          plot(predict(eval(parse(text = paste("newmodel",j,sep="")))),ydata[,j], xlab = "Predicted", ylab="Actual")
          abline(a=0,b=1, lty = 2)
          plot(predict(eval(parse(text = paste("newmodel",j,sep="")))),predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j], xlab = "Fitted", ylab = "Residuals")
          abline(a=0,b=0, lty = 2)
          plot(q$x,(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j])/sd(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j]), xlab = "Theoretical Quantile", ylab="Standardized Residual")
          abline(a=0,b=1, lty =2)
          plot(h,D, xlab = "Influence", ylab = "Cook's Distance")
        })
      })}
      #This is a test for outliers

      #clicking the predict button....
      
      observeEvent(input$predict_button,{
        
        
        
        
          filexnew<-input$NewData
          if (is.null(filexnew)){
            return(NULL)
          }
          xnewdata<-read.csv(filexnew$datapath)
          xnewcolcnt<-length(xnewdata[1,])
          xnewrowcnt<-length(xnewdata[,1])
          xnewcol_names<-colnames(xnewdata)
          tester = identical(all.equal(xnewcol_names, xcolumn_names), TRUE)
          #make sure to get same number of variables
            if (xnewcolcnt != xcolcnt){
            showModal(modalDialog(
              title = "Oops!",
              "You do not have as many independent variables as you used to build your model. Either select a new dataset to use to predict, or build a new model with the number of variables you will be using",
              easyClose = TRUE,
              footer = NULL
            ))
          }
          
            else if(tester == FALSE){
              showModal(modalDialog(
                title = "Oops!",
                "The names of your columns don't match what you used to model. Please change them so the model will make predictions",
                easyClose = TRUE,
                footer = NULL
              ))
            }
          

          #calculate the predicted values
          else{
            #show update predict button
            output$update_predict_button_placeholder<-renderUI({
              tagList(
                br(),
              actionButton("update_predict_button", label = "Update")
              )
              })
            
            prediction = matrix(0, nrow = xnewrowcnt, ncol=ycolcnt)
            for(i in 1:ycolcnt){
              prediction[,i]<-predict(eval(parse(text = paste0("newmodel",i))),xnewdata)
            }
          
            #turn it into a table
            prediction<-as.data.frame(prediction)
            colnames(prediction)<-colnames(ydata)
            xnewdata_table<-rhandsontable(xnewdata, width= 750, height = 500)
            hot_context_menu(xnewdata_table, allowRowEdit = TRUE, allowColEdit = TRUE)
            prediction_table<-rhandsontable(prediction,readOnly = TRUE,width=750,height = 500)
            
            output$xnew<-renderRHandsontable(hot_heatmap(xnewdata_table))
            output$WTF<-renderRHandsontable(hot_heatmap(prediction_table))
            
            #build x summary table
            xdata_summary<-matrix(0,nrow = 6, ncol = length(xnewdata[1,]))
            for(i in 1:length(xnewdata[1,])){
              xdata_summary[,i]<-summary(xnewdata[,i])
            }
            colnames(xdata_summary)<-colnames(xnewdata)
            rownames(xdata_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
            xdata_summary_table<-rhandsontable(xdata_summary, readOnly = TRUE)
            output$xpredictionsummary<-renderRHandsontable(hot_heatmap(xdata_summary_table))
            
            #build prediction summary table
            prediction_summary<-matrix(0,nrow = 6, ncol = ycolcnt)
            for(i in 1:ycolcnt){
              prediction_summary[,i]<-summary(prediction[,i])
            }
            colnames(prediction_summary)<-colnames(prediction)
            rownames(prediction_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
            prediction_summary_table<-rhandsontable(prediction_summary,readOnly = TRUE)
            output$ypredictionsummary<-renderRHandsontable(hot_heatmap(prediction_summary_table))
          }
          
        
        
       
        
          
        
        
      })
      observeEvent(input$update_predict_button,{
        #grab whatever data is in the table
        newnewxdata<-hot_to_r(input$xnew)
        newnewxdata_table<-rhandsontable(newnewxdata, width = 750, height = 500)
        hot_context_menu(newnewxdata_table, allowRowEdit = TRUE, allowColEdit = TRUE)
        
        #generate a new prediction
        prediction = matrix(0, nrow = length(newnewxdata[,1]), ncol=ycolcnt)
        for(i in 1:ycolcnt){
          prediction[,i]<-predict(eval(parse(text=paste0("newmodel",i))),newnewxdata)
        }
        
        #turn it into a table
        prediction<-as.data.frame(prediction)
        colnames(prediction)<-colnames(ydata)
        prediction_table<-rhandsontable(prediction,readOnly = TRUE, width = 750, height = 500)
        output$xnew<-renderRHandsontable(hot_heatmap(newnewxdata_table))
        output$WTF<-renderRHandsontable(hot_heatmap(prediction_table))
        
        #build x summary table
        newnewxdata_columnnames<- colnames(newnewxdata)
        xdata_summary<-matrix(0,nrow = 6, ncol = length(newnewxdata[1,]))
        for(i in 1:length(newnewxdata[1,])){
          xdata_summary[,i]<-summary(newnewxdata[,i])
        }
        colnames(xdata_summary)<-colnames(newnewxdata)
        rownames(xdata_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
        xdata_summary_table<-rhandsontable(xdata_summary, readOnly = TRUE)
        output$xpredictionsummary<-renderRHandsontable(hot_heatmap(xdata_summary_table))
        
        #build prediction summary table
        prediction_summary<-matrix(0,nrow = 6, ncol = ycolcnt)
        for(i in 1:ycolcnt){
          prediction_summary[,i]<-summary(prediction[,i])
        }
        colnames(prediction_summary)<-colnames(prediction)
        rownames(prediction_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
        prediction_summary_table<-rhandsontable(prediction_summary, readOnly = TRUE)
        output$ypredictionsummary<-renderRHandsontable(hot_heatmap(prediction_summary_table))
      })
    }
    #Unfortunately, when we iteratively generate ui to accommodate any size data set, we input features which remain upon subsequent clicks. Therefore we have two seperate for loops, one for the first time the button is clicked, one for the second which will remove all previous ui and then re run
     else{
       showModal(modalDialog(
         title = "Calculating...",
         "This may take some time. Please be patient.",
         easyClose = TRUE,
         footer = NULL
       ))
       #import upstream qc data
       filex<-input$x
       if (is.null(filex)){
         return(NULL)
       }
       xdata<-read.csv(filex$datapath)
       xcolcnt<-length(xdata[1,])
       xcolumn_names<-colnames(xdata)
       #import downstream data
       filey<-input$y
       if(is.null(filey)){
         return(NULL)
       }
       ylist<-read.csv(filey$datapath)
       ydata<-as.data.frame(ylist)
       ycolcnt<-length(ydata[1,])
       ycolumn_names<-colnames(ydata)
       
       #return it as an rhandsontable
       ydata_table<-rhandsontable(ydata, width = 750, height = 500, readOnly = TRUE)%>%
         hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
       xdata_table<-rhandsontable(xdata, width = 750, height = 500, readOnly = TRUE)%>%
         hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
       output$xtable<-renderRHandsontable(hot_heatmap(xdata_table, color_scale = c("#0066ff", "#ffff00")))
       output$ytable<-renderRHandsontable(hot_heatmap(ydata_table, color_scale = c("#0066ff", "#ffff00")))
       
       #Therefore this is the only new code
       #Print model summary for each dependent variable
       output$kfoldvar1<-renderPrint(newmodel1)
       for(i in 1:ycolcnt){
         local({
           x<-i
           removeUI(selector = paste0("#Validationdiv",x))
         })
       }
       
       #print variable importance for each dependent variable
       for(i in 1:ycolcnt){
         local({
           x<-i
           removeUI(selector = paste0("#variableimportancediv",x))
         })
       }
       
       #Uses a for loop to iteratively generate panels with diagnostic plots for the number of dependent variables  
       for(i in 1:ycolcnt){local({
         x<-i
         removeUI(selector= paste0("#plotdiv",x))
       })}
       #up to here
       
       
       
       
       
       
       
       #set repeated k fold cv with slider inputs
       fitControl<-trainControl(method = "repeatedcv", repeats = input$Repeats, number = input$numfolds)
       
       #compute model with full data set
       if(input$Model_select == 1){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "lm", trControl = fitControl))
         }
       }
       else if(input$Model_select == 2){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "ridge", trControl = fitControl))
         }
       }
       else if(input$Model_select == 3){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "lasso", trControl = fitControl))
         }
       }
       else if(input$Model_select == 4){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "enet", trControl = fitControl))
         }
       }
       else if(input$Model_select == 5){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "knn", trControl = fitControl))
         }
       }
       else if(input$Model_select == 7){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "gcvEarth", trControl = fitControl))
         }
       }
       else if(input$Model_select == 8){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "parRF", trControl = fitControl, importance = TRUE))
         }
       }
       else if(input$Model_select == 9){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "evtree", trControl = fitControl, importance = TRUE))
         }
       }
       else if(input$Model_select == 10){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "RRF", trControl = fitControl, importance = TRUE))
         }
       }
       else if(input$Model_select == 11){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "qrf", trControl = fitControl, importance = TRUE))
         }
       }
       else if(input$Model_select == 12){
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "xgbTree", trControl = fitControl, importance = TRUE))
         }
       }
       else{
         for(i in 1:ycolcnt){
           y<-ydata[,i]
           assign(paste0("newmodel",i),train(xdata,y, method = "svmLinear2", trControl = fitControl))
         }
       }
       
       #Instruct user to move on after model built
       showModal(modalDialog(
         title = "Congratulations!",
         "You have built a model. Click on the 'Verify' tab to explore how well it behaves. Feel free to make changes to your data or model and click on the build button again. Otherwise, start making predictions!",
         easyClose = TRUE,
         footer = NULL
       ))
       
       #Print model summary for each dependent variable
       #Print model summary for each dependent variable
       #output$kfoldvar1<-renderPrint(newmodel1)
       for(i in 1:ycolcnt){
         local({
           x<-i
           insertUI(selector = "#validation1",
                    ui = tags$div(id = paste0("Validationdiv",x),tags$h5(ycolumn_names[x]), tableOutput(paste("kfoldvar",x,sep=""))))
           output[[paste("kfoldvar",x,sep="")]]<-renderTable(print(eval(parse(text = paste("newmodel",x,sep=""))), selectCol = TRUE))
         })
       }
       
       #print variable importance for each dependent variable
       for(i in 1:ycolcnt){
         local({
           x<-i
           insertUI(selector = "#variableimportance1",
                    ui=tags$div(id = paste0("variableimportancediv",x),tags$h5(ycolumn_names[x]), verbatimTextOutput(paste("variable_importance",x,sep=""))))
           output[[paste("variable_importance",x,sep="")]]<-renderPrint(varImp(eval(parse(text = paste("newmodel",x,sep="")))))
         })
       }
       
       
       
       #Uses a for loop to iteratively generate panels with diagnostic plots for the number of dependent variables  
       #Uses a for loop to iteratively generate panels with diagnostic plots for the number of dependent variables  
       for(i in 1:ycolcnt){local({
         x<-i
         insertUI(selector= "#Plot_1", 
                  where = "beforeEnd",
                  ui=tags$div(id = paste0("plotdiv",x),tags$h5(ycolumn_names[x]), plotOutput(paste("Diagnostic_",x, sep = ""), height = "800px"))
         )
         
       })}
       for(t in 1:ycolcnt){local({
         j<-t
         #calculate theoretical quantile
         q<-qqnorm(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j])
         #Calculate influence (h)
         X<-as.matrix(xdata)
         H<-X%*%solve(t(X)%*%X)%*%t(X)
         h<-diag(H)
         #calculate components of cooks distance
         #e=error
         e<-predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j]
         #s = RMSE
         s<-eval(parse(text = paste("newmodel",j,"$results$RMSE",sep="")))
         #p=no. independent vars
         p<-length(xdata[1,])
         #Cooks Distance
         D<-e*e/(s*s*p)*(h/((1-h)*(1-h)))
         output[[paste("Diagnostic_",j,sep="")]]<-renderPlot({
           par(mfrow=c(2,2))
           plot(predict(eval(parse(text = paste("newmodel",j,sep="")))),ydata[,j], xlab = "Predicted", ylab="Actual")
           abline(a=0,b=1, lty = 2)
           plot(predict(eval(parse(text = paste("newmodel",j,sep="")))),predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j], xlab = "Fitted", ylab = "Residuals")
           abline(a=0,b=0, lty = 2)
           plot(q$x,(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j])/sd(predict(eval(parse(text = paste("newmodel",j,sep=""))))-ydata[,j]), xlab = "Theoretical Quantile", ylab="Standardized Residual")
           abline(a=0,b=1, lty =2)
           plot(h,D, xlab = "Influence", ylab = "Cook's Distance")
         })
       })}

       
       #clicking the predict button....
       
       observeEvent(input$predict_button,{
         
         
         #If the button is clicked the first time....
         
           filexnew<-input$NewData
           if (is.null(filexnew)){
             return(NULL)
           }
           xnewdata<-read.csv(filexnew$datapath)
           xnewcolcnt<-length(xnewdata[1,])
           xnewrowcnt<-length(xnewdata[,1])
           xnewcol_names<-colnames(xnewdata)
           xcolumn_names<-colnames(xdata)
           ycolumn_names<-colnames(ydata)
           tester<-identical(all.equal(xnewcol_names, xcolumn_names), TRUE)
           #make sure to get same number of variables with the same names
           
             if (xnewcolcnt != xcolcnt){
               showModal(modalDialog(
                 title = "Oops!",
                 "You do not have as many independent variables as you used to build your model. Either select a new dataset to use to predict, or build a new model with the number of variables you will be using",
                 easyClose = TRUE,
                 footer = NULL
               ))
             }
             
             else if(tester == FALSE){
               showModal(modalDialog(
                 title = "Oops!",
                 "The names of your columns don't match what you used to model. Please change them so the model will make predictions",
                 easyClose = TRUE,
                 footer = NULL
               ))
             }
             
             
             #calculate the predicted values
             else{
               #show update predict button
               output$update_predict_button_placeholder<-renderUI({
                 tagList(
                   br(),
                   actionButton("update_predict_button", label = "Update")
                 )
               })
               
               prediction = matrix(0, nrow = xnewrowcnt, ncol=ycolcnt)
               for(i in 1:ycolcnt){
                 prediction[,i]<-predict(eval(parse(text = paste0("newmodel",i))),xnewdata)
               }
               
               #turn it into a table
               prediction<-as.data.frame(prediction)
               colnames(prediction)<-colnames(ydata)
               xnewdata_table<-rhandsontable(xnewdata, width= 750, height = 500)
               hot_context_menu(xnewdata_table, allowRowEdit = TRUE, allowColEdit = TRUE)
               prediction_table<-rhandsontable(prediction,readOnly = TRUE,width=750,height = 500)
               
               output$xnew<-renderRHandsontable(hot_heatmap(xnewdata_table))
               output$WTF<-renderRHandsontable(hot_heatmap(prediction_table))
               
               #build x summary table
               xdata_summary<-matrix(0,nrow = 6, ncol = length(xnewdata[1,]))
               for(i in 1:length(xnewdata[1,])){
                 xdata_summary[,i]<-summary(xnewdata[,i])
               }
               colnames(xdata_summary)<-colnames(xnewdata)
               rownames(xdata_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
               xdata_summary_table<-rhandsontable(xdata_summary, readOnly = TRUE)
               output$xpredictionsummary<-renderRHandsontable(hot_heatmap(xdata_summary_table))
               
               #build prediction summary table
               prediction_summary<-matrix(0,nrow = 6, ncol = ycolcnt)
               for(i in 1:ycolcnt){
                 prediction_summary[,i]<-summary(prediction[,i])
               }
               colnames(prediction_summary)<-colnames(prediction)
               rownames(prediction_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
               prediction_summary_table<-rhandsontable(prediction_summary,readOnly = TRUE)
               output$ypredictionsummary<-renderRHandsontable(hot_heatmap(prediction_summary_table))
             }
           
         
         
         #If the button is clicked again...
         
           
         
         
       })
       observeEvent(input$update_predict_button,{
         #grab whatever data is in the table
         newnewxdata<-hot_to_r(input$xnew)
         newnewxdata_table<-rhandsontable(newnewxdata, width = 750, height = 500)
         hot_context_menu(newnewxdata, allowRowEdit = TRUE, allowColEdit = TRUE)
         #generate a new prediction
         prediction = matrix(0, nrow = length(newnewxdata[,1]), ncol=ycolcnt)
         for(i in 1:ycolcnt){
           prediction[,i]<-predict(eval(parse(text=paste0("newmodel",i))),newnewxdata)
         }
         
         #turn it into a table
         prediction<-as.data.frame(prediction)
         colnames(prediction)<-colnames(ydata)
         prediction_table<-rhandsontable(prediction,readOnly = TRUE, width = 750, height = 500)
         output$xnew<-renderRHandsontable(hot_heatmap(newnewxdata_table))
         output$WTF<-renderRHandsontable(hot_heatmap(prediction_table))
         
         #build x summary table
         newnewxdata_columnnames<- colnames(newnewxdata)
         xdata_summary<-matrix(0,nrow = 6, ncol = length(newnewxdata[1,]))
         for(i in 1:length(newnewxdata[1,])){
           xdata_summary[,i]<-summary(newnewxdata[,i])
         }
         colnames(xdata_summary)<-colnames(newnewxdata)
         rownames(xdata_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
         xdata_summary_table<-rhandsontable(xdata_summary, readOnly = TRUE)
         output$xpredictionsummary<-renderRHandsontable(hot_heatmap(xdata_summary_table))
         
         #build prediction summary table
         prediction_summary<-matrix(0,nrow = 6, ncol = ycolcnt)
         for(i in 1:ycolcnt){
           prediction_summary[,i]<-summary(prediction[,i])
         }
         colnames(prediction_summary)<-colnames(prediction)
         rownames(prediction_summary)<-c("Min","Q1","Median","Mean","Q3","Max")
         prediction_summary_table<-rhandsontable(prediction_summary, readOnly = TRUE)
         output$ypredictionsummary<-renderRHandsontable(hot_heatmap(prediction_summary_table))
       })
       }
  })
  
  
  
  
})

