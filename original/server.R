library(sqldf)
library(rvest)
library(httr)
library(ggplot2)
library(dplyr)
library(zoo)

shinyServer(function(input, output){

  runTimeData <- reactive({
    timeThreshold = paste(input$timeThresholdInput, ":00", sep="")
    
    allRunsURL <- paste("http://www.parkrun.org.uk/results/athleteeventresultshistory/?athleteNumber=", 
                        input$athleteIDInput,
                        "&eventNumber=0",
                        sep="")
    allRunsPage <- read_html(GET(allRunsURL, add_headers('user-agent' = 'r')))
    allRunsDF <- allRunsPage %>%
      html_node("div:nth-child(10) #results") %>%
      html_table()
    
    colnames(allRunsDF) <- c("Event", "RunDate", "RunNumber", "Position", "Time", "AgeGrade", "PB")
    
    allRunsDF$RunDate <- as.Date(allRunsDF$RunDate, "%d/%m/%Y")
    
    firstRunBelowThreshold <- sqldf(paste("SELECT min(RunDate) AS firstRunBelowThreshold__Date, '",
                                          timeThreshold,
                                          "' AS timeThreshold FROM allRunsDF WHERE Time < '",
                                          timeThreshold,
                                          "'",
                                          sep = ""),
                                    method = "name__class")
    
    allRunsDF2 <- merge(allRunsDF, firstRunBelowThreshold)
    
    allRunsDF3 <- mutate(allRunsDF2, filterOut = factor(case_when(Time > timeThreshold &
                                                                    RunDate > firstRunBelowThreshold ~ "Exclude",
                                                                  TRUE ~ "Include" )))
    allRunsDF3$Time <- as.POSIXct(allRunsDF3$Time, format="%M:%S")
    
    allRunsDF3
  })
  
  runTimeScatterPlot <- reactive({
    newData <- runTimeData()
    ggplot(newData, aes(RunDate, Time)) + geom_point(aes(colour = factor(filterOut))) + guides(fill=guide_legend(title="Legend"))
  })

  weightVsSpeedPlot <- reactive({
    if(is.null(input$weightFileInput$name)) {
      
    } else {
      allRunsData <- runTimeData()
      
      weightHistory <- read.csv(input$weightFileInput$datapath)
      weightHistory$date <- as.Date(weightHistory$date)
      
      z <- read.zoo(data.frame(date = weightHistory$date,
                               weight = weightHistory$weight))
      zz <- na.approx(z, xout = seq(start(z), end(z), "day"))
      
      finalWeightHistory <- fortify.zoo(zz)
      colnames(finalWeightHistory) <- c("date", "weight")
      
      weightVsSpeed <- merge(allRunsData, finalWeightHistory, by.x = "RunDate", by.y = "date")
      weightVsSpeedFiltered <- weightVsSpeed[weightVsSpeed$filterOut == "Include",]
      ggplot(weightVsSpeedFiltered, aes(weight, Time)) + geom_point() + geom_smooth()
    }
  })
  
  output$scatterPlotOutput <- renderPlot(
    runTimeScatterPlot()
  )
  output$weightSpeedPlotOutput <- renderPlot(
    weightVsSpeedPlot()
  )
})
