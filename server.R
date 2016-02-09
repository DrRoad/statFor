# server.R

rm(list = ls()); cat("\014"); graphics.off(); # Clear Workspace
library(devtools)
library(forecast); 
library(rminer); 
library(R.matlab) ;
library(scales); 
library(timeSeries); 
library(ggplot2); 
library(plyr); 
library(zoo); 
library(xts);
library(matrixStats) # Load Packages
library(dplyr); 
library(numDeriv); 
library(astsa); 
library(tsDyn)
library(raster); 
library(rgdal);
library(ggmap); 
library(hddtools)
#library(RNetCDF)
library(rts)
library(rgeos)
#library(ncdf4)
library(nnet)
#devtools::install_github("hydrosolutions/ftimeseries")
library(ftimeseries)  # hydrosolutions time series forecast library.

# Get data.
# Discharge
qZ <- read.csv(file="data/stations/Zhamashike_runoff_00_14.csv", header=TRUE, sep=",")
qQ <- read.csv(file="data/stations/Qilian_runoff_00_14.csv",header=TRUE,sep=",")
startDate <- c(2000,01,01)

# Default model parameters
error.test <- c("MAE", "RMSE")
ensembleSize <- 1
model.type <- 'mlpe'
test.for.fit <- 'MAE'
load("./data/qQ.mlpe.fit.RData")

# Pre-processing for display of input data.
qZts <- ftimeseries:::readAndProcessData(qZ,startDate)
qQts <- ftimeseries:::readAndProcessData(qQ,startDate)




shinyServer(
  function(input, output, session) {
    
    # Map stuff
    output$mapText <- renderText({
      outtext = switch(input$map_data,
                       "Topography" = paste("Topographical map of the model area."))
    })
    
    # Send a pre-rendered image, and don't delete the image after sending it
    output$preImage <- renderImage({
      filename <- switch(input$map_data,
                         "Topography" = list("topo.pdf")
      )
      
      filename <- normalizePath(file.path('./images',filename))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("Topographical map of the model area.", input$map_data))
    }, deleteFile = FALSE)
    
    # Time series data stuff
    updateSelectizeInput(
      session, 
      "dischargeStation", 
      choices = c("Qilian",
                  #"Yingluoxia",
                  "Zhamashike"),
      server = TRUE
    )
    output$timeSeriesDataText <- renderText({ 
      outtext = switch(input$data,
                       "Precipitation" = paste("Precipitation raster map."),
                       "Temperature" = paste("Temperature data."),
                       "Discharge" = paste("Discharge time series. The data of the selected station is drawn in red."),
                       "Snow cover" = paste("Snow cover data."))
    })
    #output$plotDailyDischarge <- renderPlot({
    #  par(mar = c(2,4,2,0.1))
    #  plot(qZDa, lty = 1, col = "gray", 
    #       ylab = 'Discharge [m3/s]')
    #  lines(qQDa, lty = 1, col = 'gray')
    #  #lines(qZDa, lty = 1, col = 'gray')
    #  stationName <- input$dischargeStation
    #  if (stationName != '') {
    #    variableName = 'qQDa'  # initialize
    #    variableName <- switch(stationName,
    #                           "Qilian" = 'qQDa',
    #                           "Yingluoxia" = 'qYDa',
    #                           "Zhamashike" = 'qZDa')
    #    lines(get(variableName),lty=1,col='red')
    #  }
    #})  #output$plotDailyDischarge
    #output$plotWeeklyDischarge <- renderPlot({
    #  par(mar = c(2,4,2,0.1))
    #  plot(qZWe, lty = 1, col = "gray", ylab = "Discharge [m3/d]")
    #  lines(qQWe, lty = 1, col = 'gray')
    #  #lines(qZWe, lty = 1, col = 'gray')
    #  stationName <- input$dischargeStation
    #  if (stationName != '') {
    #    variableName = 'qQWe'  # initialize
    #    variableName <- switch(stationName,
    #                           "Qilian" = 'qQWe',
    #                           "Yingluoxia" = 'qYWe',
    #                           "Zhamashike" = 'qZWe')
    #    lines(get(variableName),lty=1,col='red')
    #  }
    #}) 
    output$plotMonthlyDischarge <- renderPlot({
      par(mar = c(2,4,2,0.1))
      plot(qZts[[1]], lty = 1, col = "gray", ylab = "Discharge [m3/d]",xlab = "Time [year]")
      lines(qQts[[1]], lty = 1, col = 'gray')
      #lines(qZMo, lty = 1, col = 'gray')
      stationName <- input$dischargeStation
      if (stationName != '') {
        variableName = 'qQts[[1]]'  # initialize
        variableName <- switch(stationName,
                               "Qilian" = 'qQts',
                               "Yingluoxia" = 'qYMo',
                               "Zhamashike" = 'qZts')
        lines(get(variableName)[[1]],lty=1,col='red')
      }
    }) 
    
    # Model configuration stuff.
    predictionQilian <- reactive({
      #ftimeseries:::predictDischargeWithDischarge(qQ,startDate,input$ensembleSize,input$model.type,input$test.for.fit,error.test)
      ftimeseries:::predictDischargeWithDischarge2(qQ,startDate,qQ.mlpe,input$ensembleSize,input$model.type,input$test.for.fit,error.test)
    })
    predictionZhamashike <- reactive({
      ftimeseries:::predictDischargeWithDischarge(qZ,startDate,input$ensembleSize,input$model.type,input$test.for.fit,error.test)
    })
    output$modelTypeText <- renderText({
      outtext = switch(input$model.type,
                       'mlpe' = paste("The model of choice is the multy-layer perceptron model.", 
                                      "The multi-layer perceptron model is a feedforward artificial neural network model.",
                                      sep = "\n"))
    })
    output$qualityMetricText <- renderText({
      outtext = switch(input$test.for.fit,
                       "MAE" = paste("You chose mean average error as quality criteria."))
    })
    output$ensembleText <- renderText({
      outtext = paste(input$ensembleSize," models are run in sequence.",sep="")
    })
    output$scatterPlotHeaderText <- renderText({
      outtext = paste("Scatter plot of the 5 years test set from 2010 to 2014.")
    })
    output$testSetTimeSeriesHeaderText <- renderText({
      outtext = paste("Observed (black) and predicted (blue) discharge at Qilian station from January 2010 to December 2014.")
    })
    output$scatterPlotQilian <- renderPlot({
      qQ           <- qQ # input at top
      qQ           <- qQ[,-1]
      qQ           <- as.vector(as.matrix(qQ))
      qQ           <- na.omit(qQ)
      start.date   <- startDate
      qQts         <- dwmTimeseries(qQ,start.date[1],start.date[2],start.date[3],"mean")
      qQ           <- qQts[[3]]
      qQp <- predictionQilian()
      # Get correlation of data.
      correlation.of.data <- NULL
      qQ.acf       <- acf(qQ,lag.max = 15)
      for(i in 1:length(qQ.acf$acf)){
        if((qQ.acf$acf[i] > 0.3)|(qQ.acf$acf[i] < -0.3)){correlation.of.data <-c(correlation.of.data,i)}}
      qQ.lag       <- CasesSeries(qQ,correlation.of.data)
      
      # Plot for one-step prediction ----
      predD        <- vector("list",1)
      predD$pred   <- qQp[[1]]
      err.mean     <- qQp[[2]]
      h            <- holdout(qQ.lag$y, ratio = 2/3, mode = "order", seed = 12345) # just for ID later on!
      ftimeseries:::ensScatterPlot(predD,qQ.lag,h,2,paste("mlpe: Qilian Monthly, MAE",
                                                          toString(signif(err.mean[1], digits = 4)),
                                                          ", R22", toString(signif(err.mean[2], digits = 4)), 
                                                          ", RMSE",toString(signif(err.mean[3], digits = 4))),
                                   "observation [m3/s]","forecast [m3/s]",input$ensembleSize)
      
    })
    output$testSetTimeSeries <- renderPlot({
      qQ           <- qQ # input at top
      qQ           <- qQ[,-1]
      qQ           <- as.vector(as.matrix(qQ))
      qQ           <- na.omit(qQ)
      start.date   <- startDate
      qQts         <- dwmTimeseries(qQ,start.date[1],start.date[2],start.date[3],"mean")
      qQ           <- qQts[[3]]
      correlation.of.data <- NULL
      qQ.acf       <- acf(qQ,lag.max = 15)
      for(i in 1:length(qQ.acf$acf)){
        if((qQ.acf$acf[i] > 0.3)|(qQ.acf$acf[i] < -0.3)){correlation.of.data <-c(correlation.of.data,i)}}
      qQ.lag       <- CasesSeries(qQ,correlation.of.data)
      nRuns        <- input$ensembleSize
      
      qQp <- predictionQilian()
      predD$pred   <- qQp[[1]]
      err.mean     <- qQp[[2]]
      
      Target       <- vector("list",nRuns)
      for(i in 1:nRuns){
        Target[[i]]      <- qQ.lag$y[(length(qQ.lag$y)*2/3+1):length(qQ.lag$y)]
      }
      pred         <- vector("list",nRuns)
      test         <- vector("list",nRuns)
      L            <- vector("list",1)
      L[[1]]       <- list(pred=predD$pred,test=Target,runs=nRuns)
      mgraph       (L,graph="REG",Grid=10,col=c("black","blue"))
    })
        
    # Forecast stuff.
    #output$predictionTextZhamashike <- renderText({
    #  qZp <- predictionZhamashike()
    #  qZp.m <- mean(unlist(qZp[1]))
    #  qZp.s <- sd(unlist(qZp[1]))
    #  outtext = paste("Predicted average montly discharge for Zhamashike in",
    #                  qZp[[2]],
    #                  "is",
    #                  qZp.m,
    #                  "m3/s with a standard deviation of",
    #                  qZp.s,
    #                  "m3/s.",
    #                  sep=" ")
    #})
    output$predictionTextQilian <- renderText({
      qQp <- predictionQilian()
      qQpEns <- qQp[[1]]
      temp <- 0
      for (i in 1:input$ensembleSize){
        temp = temp + qQpEns[[i]][input$target]
      }
      qQpTarget <- temp/input$ensembleSize
      qQpError <- qQp[[2]]
      #qQp.m <- mean(unlist(qQp[1]))
      #qQp.s <- sd(unlist(qQp[1]))
      monthString <- switch(input$target,
                            "1" = "January 2000",
                            "2" = "February 2000",
                            "3" = "March 2000",
                            "4" = "April 2000",
                            "5" = "May 2000")
      outtext = paste("Predicted average montly discharge for Qilian in",
                      #qQp[[2]],
                      monthString,
                      "is",
                      qQpTarget,
                      "m3/s with an expected Error of",
                      qQpError[1],
                      "m3/s.",
                      sep=" ")
    })
    #output$predictionTextSum <- renderText({
    #  qQp <- predictionQilian()
    #  qQp.m <- mean(unlist(qQp[1]))
    #  qQp.s <- sd(unlist(qQp[1]))
    #  qZp <- predictionZhamashike()
    #  qZp.m <- mean(unlist(qZp[1]))
    #  qZp.s <- sd(unlist(qZp[1]))
    #  outtext = paste("The inflow to the gorge in the month",
    #                  qZp[[2]],
    #                  "is predicted to be an average of",
    #                  (qZp.m + qQp.m),
    #                  "m/s.",
    #                  sep = " ")
    #})
    output$predictionPlotQilian <- renderPlot({
      qQp <- predictionQilian()
      qQp.m <- mean(unlist(qQp[[1]]))
      qQp.s <- mean(unlist(qQp[[1]]))
      plot(qQts[[1]], lty = 1, col = "black", ylab = "Discharge Qilian [m3/d]")
      t = time(qQts[[1]])
      x = c(tail(t,n=1),(tail(t,n=1)+diff(tail(t,n=2))))
      y = c(tail(qQts[[1]],n=1),qQp.m+2*qQp.s)
      lines(x,y,lty=1,col='gray')
      y = c(tail(qQts[[1]],n=1),qQp.m-2*qQp.s)
      lines(x,y,lty=1,col='gray')
      y = c(tail(qQts[[1]],n=1),qQp.m)
      lines(x,y, lty = 1, col = 'blue')
    })
    #output$predictionPlotZhamashike <- renderPlot({
    #  qZp <- predictionZhamashike()
    #  qZp.m <- mean(unlist(qZp[1]))
    #  qZp.s <- sd(unlist(qZp[1]))
    #  plot(qZts[[1]], lty = 1, col = "black", ylab = "Discharge Zhamashike [m3/d]")
    #  t = time(qZts[[1]])
    #  x = c(tail(t,n=1),(tail(t,n=1)+diff(tail(t,n=2))))
    #  y = c(tail(qZts[[1]],n=1),qZp.m+2*qZp.s)
    #  lines(x,y,lty=1,col='gray')
    #  y = c(tail(qZts[[1]],n=1),qZp.m-2*qZp.s)
    #  lines(x,y,lty=1,col='gray')
    #  y = c(tail(qZts[[1]],n=1),qZp.m)
    #  lines(x,y, lty = 1, col = 'blue')
    #  })
    
    
    
  }
)
    