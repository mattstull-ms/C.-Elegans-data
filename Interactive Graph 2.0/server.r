library(shiny)
library(rflot)
library(rhandsontable)
shinyServer(function(input, output) {
  

  #Perform this action after build button clicked
  observeEvent(input$Go,{
    #input csv file into data frame
    filex<-input$File_1
    if (is.null(filex)){
      return(NULL)
    }
    xdata<-read.csv(filex$datapath)
    xdata<-as.data.frame(xdata)
    RQN<-as.data.frame(xdata$RQN)
    #source image files
    for(i in 1:length(xdata[,1])){
      RQN[i,1]<-paste0("<a href ='https://raw.githubusercontent.com/mattstull-ms/iQC-graph/master/16118kt_",i,".PNG', target = '_blank'>Link</a>")
    }
   colnames(RQN)<-"Fragment analyzer Trace"
    
   #Rendering Interactive graph
    output$graph1<-renderFlot({
      flotChart(xdata) %>%
        #set series options 
         flotSeries(x=Concentration, y=RQN, label = "", group = Group, 
                     extra.cols = list(Sample,X.260.280.,X.260.230.), 
                     clickable = TRUE, hoverable = TRUE, points = list(show = TRUE))%>%
      #set axis options/enable hover click
        flotOptions(
        xaxis = list(min = 0),
        yaxis = list(min = 0, max =10.5),
          tooltipOpts = list( id = "customTip", 
                            #This returns the tooltip
                                  content = htmlwidgets::JS("
                                                function(label, xval, yval, flotItem){
                                                  return 'Sample: ' + flotItem.series.extra_data[flotItem.dataIndex][0] + 
                                                    '<br>RQN: ' + flotItem.series.data[flotItem.dataIndex][1] +
                                                    '<br>Concentration: ' + flotItem.series.data[flotItem.dataIndex][0] + 'ng/ul' +
                                                    '<br>260/280: ' + flotItem.series.extra_data[flotItem.dataIndex][1]+
                                                    '<br>260/230: ' + flotItem.series.extra_data[flotItem.dataIndex][2];
                                                }")
                            ),
        #Display some information when clicking a point using jquery and html. Trickier than it looks
        onClick = htmlwidgets::JS("
                                  function (event, pos, flotItem) {
                                 
                                  if (flotItem) {
                                    
                                    $('#clickp').html('<p id = '+ '\"thisp\"' + '>'+
                                                        '<span>'+
                                                        '<b>Sample No.</b> ' + flotItem.series.extra_data[flotItem.dataIndex][0] + 
                                                        '<br><b>RQN:</b> ' +flotItem.series.data[flotItem.dataIndex][1] + 
                                                        '<br><b>Concentration:</b> ' + flotItem.series.data[flotItem.dataIndex][0] + 'ng/ul' +
                                                        '<br><b>260/280:</b> ' + flotItem.series.extra_data[flotItem.dataIndex][1]+
                                                        '<br><b>260/230:</b> ' + flotItem.series.extra_data[flotItem.dataIndex][2] + 
                                                          '</span>'+'</p>');
                                $('#thisp').css('border-style','inset');
                                  }}"),
        #set threshhold values               
        grid = htmlwidgets::JS("
                               {
				markings:[
            {xaxis: { from: 0, to: 50}, color: '#C11B17'},
            {yaxis: { from: 0, to: 4}, color: '#C11B17'}
                ],
        clickable: true,
        hoverable: true
                            }")
                            )

          
    })
    #Compute some statistics and output a table
    av<-aggregate(xdata[,5], list(xdata$Group), mean)
    st_dev<-aggregate(xdata[,5], list(xdata$Group), sd)
    groupdata<-split(xdata[,5],xdata$Group)
    stat_data<-cbind(av[,2], st_dev[,2])
    stat_data<-as.data.frame(stat_data)
    colnames(stat_data)<-c("Mean", "Standard Deviation")
    row.names(stat_data)<-av[,1]
    rstat<-rhandsontable(stat_data, readOnly = TRUE)%>%
    hot_cols(renderer = htmlwidgets::JS("safeHtmlRenderer"))
    output$Stat_table<-renderRHandsontable(hot_table(rstat, rowHeaderWidth = 150))
    #make a table
    tab<-cbind(xdata[,1:6], RQN)
    output$StatHeader<-renderUI(h4("Quality Statitics"))
    rtab<-rhandsontable(data = tab, rowHeaders = FALSE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
    #color RQN below 4
      rtab<-hot_col(rtab,5, renderer = htmlwidgets::JS("function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
                    if (value<=4) {
                    td.style.background = 'red';
                    } 
  }"))
      #color concentration below 1000
     rtab<-hot_col(rtab,2, renderer = htmlwidgets::JS("function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                                      if (value<=50) {
                                                      td.style.background = 'red';
                                                      } 
  }"))
     rtab<-hot_col(rtab,6,type = "autocomplete")
     #safely turn string into a hyperlink
     rtab<-hot_col(rtab, 7,renderer = htmlwidgets::JS("safeHtmlRenderer"))
     
     #enable column sorting
     rtab<-hot_cols(rtab,columnSorting = TRUE)
    #Generate UI to enable further actions
     output$table1header<-renderUI(list(h4("Data Table"), p("Make changes to the table and click 'Update' to see what happens to the graph"), actionButton("updateButton", label = "Update"), br()))
     #output the table
     output$table1<-renderRHandsontable(rtab)
  })
  
  
#Clicking the updata button
  observeEvent(input$updateButton,{
    xdata<-hot_to_r(input$table1)
    RQN<-as.data.frame(xdata$RQN)
    #source image files
    for(i in 1:length(xdata[,1])){
      RQN[i,1]<-paste0("<a href ='https://raw.githubusercontent.com/mattstull-ms/iQC-graph/master/16118kt_",i,".PNG', target = '_blank'>Link</a>")
    }
    colnames(RQN)<-"Fragment analyzer Trace"
    
    #Rendering Interactive graph
    output$graph1<-renderFlot({
      flotChart(xdata) %>%
        #set series options 
        flotSeries(x=Concentration, y=RQN, label = "", group = Group, 
                   extra.cols = list(Sample,X.260.280.,X.260.230.), 
                   clickable = TRUE, hoverable = TRUE, points = list(show = TRUE))%>%
        #set axis options/enable hover click
        flotOptions(
          xaxis = list(min = 0),
          yaxis = list(min = 0, max =10.5),
          tooltipOpts = list( id = "customTip", 
                              #This returns the tooltip
                              content = htmlwidgets::JS("
                                                        function(label, xval, yval, flotItem){
                                                        return 'Sample: ' + flotItem.series.extra_data[flotItem.dataIndex][0] + 
                                                        '<br>RQN: ' + flotItem.series.data[flotItem.dataIndex][1] +
                                                        '<br>Concentration: ' + flotItem.series.data[flotItem.dataIndex][0] + 'ng/ul' +
                                                        '<br>260/280: ' + flotItem.series.extra_data[flotItem.dataIndex][1]+
                                                        '<br>260/230: ' + flotItem.series.extra_data[flotItem.dataIndex][2];
                                                        }")
                            ),
          #Display some information when clicking a point using jquery and html. Trickier than it looks
          onClick = htmlwidgets::JS("
                                    function (event, pos, flotItem) {
                                    
                                    if (flotItem) {
                                    
                                    $('#clickp').html('<p id = '+ '\"thisp\"' + '>'+
                                    '<span>'+
                                    '<b>Sample No.</b> ' + flotItem.series.extra_data[flotItem.dataIndex][0] + 
                                    '<br><b>RQN:</b> ' +flotItem.series.data[flotItem.dataIndex][1] + 
                                    '<br><b>Concentration:</b> ' + flotItem.series.data[flotItem.dataIndex][0] + 'ng/ul' +
                                    '<br><b>260/280:</b> ' + flotItem.series.extra_data[flotItem.dataIndex][1]+
                                    '<br><b>260/230:</b> ' + flotItem.series.extra_data[flotItem.dataIndex][2] + 
                                    '</span>'+'</p>');
                                    $('#thisp').css('border-style','inset');
                                    }}"),
        #set threshhold values               
        grid = htmlwidgets::JS("
                               {
                               markings:[
                               {xaxis: { from: 0, to: 50}, color: '#C11B17'},
                               {yaxis: { from: 0, to: 4}, color: '#C11B17'}
                               ],
                               clickable: true,
                               hoverable: true
                               }")
                            )
      
      
    })
    #Compute some statistics and output a table
    av<-aggregate(xdata[,5], list(xdata$Group), mean)
    st_dev<-aggregate(xdata[,5], list(xdata$Group), sd)
    groupdata<-split(xdata[,5],xdata$Group)
    stat_data<-cbind(av[,2], st_dev[,2])
    stat_data<-as.data.frame(stat_data)
    colnames(stat_data)<-c("Mean", "Standard Deviation")
    row.names(stat_data)<-av[,1]
    rstat<-rhandsontable(stat_data, readOnly = TRUE)%>%
      hot_cols(renderer = htmlwidgets::JS("safeHtmlRenderer"))
    output$Stat_table<-renderRHandsontable(hot_table(rstat, rowHeaderWidth = 150))
    #make a table
    tab<-cbind(xdata[,1:6], RQN)
    output$StatHeader<-renderUI(h4("Quality Statitics"))
    rtab<-rhandsontable(data = tab, rowHeaders = FALSE, renderer = htmlwidgets::JS("safeHtmlRenderer"))
    #color RQN below 4
    rtab<-hot_col(rtab,5, renderer = htmlwidgets::JS("function (instance, td, row, col, prop, value, cellProperties) {
                                                     Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                                     if (value<=4) {
                                                     td.style.background = 'red';
                                                     } 
                                                     }"))
      #color concentration below 1000
    rtab<-hot_col(rtab,2, renderer = htmlwidgets::JS("function (instance, td, row, col, prop, value, cellProperties) {
                                                     Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                                     if (value<=50) {
                                                     td.style.background = 'red';
                                                     } 
                                                     }"))
     rtab<-hot_col(rtab,6,type = "autocomplete")
     #safely turn string into a hyperlink
     rtab<-hot_col(rtab, 7,renderer = htmlwidgets::JS("safeHtmlRenderer"))
     
     #enable column sorting
     rtab<-hot_cols(rtab,columnSorting = TRUE)
     #Generate UI to enable further actions
     output$table1header<-renderUI(list(h4("Data Table"), p("Make changes to the table and click 'Update' to see what happens to the graph"), actionButton("updateButton", label = "Update"), br()))
     #output the table
     output$table1<-renderRHandsontable(rtab)
     })
})
