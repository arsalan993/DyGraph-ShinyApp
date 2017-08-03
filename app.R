options(java.parameters = "-Xmx8000m")
library("openxlsx")
library(chron)
library(zoo)
library(xts)
library(dygraphs)

#https://stackoverflow.com/questions/39348005/shiny-progress-bar-appearing-prematurely



ui <- fluidPage(
  fluidRow(
    titlePanel("Concrete Resistance Logger Virtualization"),
    column(
        fileInput('file1', 'Kindly upload the CSV  file here',
                  accept = c(".csv")
        ),width = 10
      ),
    column(dygraphOutput("plot1"), width=10),
    column(textOutput("legendDivID1"), title = "Legend", collapsible = TRUE, width=2),
    column(dygraphOutput("plot2"), width=10),
    column(textOutput("legendDivID2"), title = "Legend", collapsible = TRUE, width=2),
    column(dygraphOutput("plot3"), width=10),
    column(textOutput("legendDivID3"), title = "Legend", collapsible = TRUE, width=2),
    column(dygraphOutput("plot4"), width=10),
    column(textOutput("legendDivID4"), title = "Legend", collapsible = TRUE, width=2)
    
    
  )
)
options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output) {
  d_f <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".csv", sep=","))
  
                   my_df <-read.csv(paste(inFile$datapath, ".csv", sep=","), stringsAsFactors = FALSE, header = F)
                 
    my_df <- my_df[-1,]
    
    colnames(my_df) = my_df[1,]
    
    my_df <- my_df[-1,]
    return(my_df)
  })
 
  dates <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    DateTime <- d_f()[c(3,4)]
    
    DateTime$Time <-chron(times =DateTime$Time)
    
    newDatetime <-paste(DateTime$Day,DateTime$Time)
    dates <-as.POSIXlt(newDatetime,format = ("%m/%d/%Y %H:%M:%S"),tz="GMT")
    return(dates)
  })
  
  df <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    plot1 =  d_f()[c(5,6,7,8,9,10,11)]
    plot1 <- data.frame(sapply(plot1, function(x) as.numeric(as.character(x))))
    df <-na.omit(cbind(dates(),plot1))
    df <- xts(df,order.by = df$dates,tz="GMT")
    return(df)
    })
  
  df1 <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    plot2 =  d_f()[c(15,16,17,18,19,20,21)]
    plot2 <- data.frame(sapply(plot2, function(x) as.numeric(as.character(x))))
      #Plot2 Data Frame
  df1 <-na.omit(cbind(dates(),plot2))
  df1 <- xts(df1,order.by = df1$dates,tz="GMT")
  return(df1)
  })
  
  df2 <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    plot3 =  d_f()[c(29,30,31)]
    plot3 <- data.frame(sapply(plot3, function(x) as.numeric(as.character(x))))
   #plot3 Data Frame
  df2 <-na.omit(cbind(dates(),plot3))
  df2 <- xts(df2,order.by = df2$dates,tz="GMT")
  return(df2)
  })
  
  df3 <- reactive({ 
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    plot4 =  d_f()[c(32,33,34)]
    plot4 <- data.frame(sapply(plot4, function(x) as.numeric(as.character(x))))
     #Plot4 Data Frame
  df3 <-na.omit(cbind(dates(),plot4))
  df3 <- xts(df3,order.by = df3$dates,tz="GMT")

  return(df3)
  })
  
  # -------------------------------------------------------------------

  output$plot1 <- renderDygraph({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    if (is.null(df()))
      return(NULL)
    withProgress(message = 'Please Wait 1',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.15)
                   }
                   #max(dat, na.rm=TRUE) dyAxis("y", valueRange = c(0, max(df(), na.rm=TRUE)))
    dygraph(df(), main = "PLOT 1") %>% dyOptions(useDataTimezone = TRUE) %>% dyRangeSelector() %>%
      dyLegend(labelsDiv = "legendDivID1") %>% dyAxis("y", valueRange = c(0,90000))
                 })   
  })
  
  # -------------------------------------------------------------------
  
  output$plot2 <- renderDygraph({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    if (is.null(df1()))
      return(NULL)
    withProgress(message = 'Please Wait 2',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.15)
                   }
    dygraph(df1(), main = "PLOT 2") %>% dyOptions(useDataTimezone = TRUE) %>% dyRangeSelector() %>%
      dyLegend(labelsDiv = "legendDivID2")
                 })        
  })
  
  # -------------------------------------------------------------------
  
  output$plot3 <- renderDygraph({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    if (is.null(df2()))
      return(NULL)
    withProgress(message = 'Please Wait3',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.15)
                   }
    dygraph(df2(), main = "PLOT 3") %>% dyOptions(useDataTimezone = TRUE) %>% dyRangeSelector() %>%
      dyLegend(labelsDiv = "legendDivID3")
                 })    
  })
  
  # -------------------------------------------------------------------
  
  output$plot4 <- renderDygraph({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    if (is.null(df3()))
      return(NULL)
    withProgress(message = 'Please Wait 4',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.15)
                   }
    dygraph(df3(),main = "PLOT 4") %>% dyOptions(useDataTimezone = TRUE) %>% dyRangeSelector() %>%
      dyLegend(labelsDiv = "legendDivID4")
                 })     
  })
  
  #---------------------------------------------------------------------------------------------
}

shinyApp(ui, server)