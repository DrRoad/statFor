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
devtools::install_github("hydrosolutions/ftimeseries")
library(ftimeseries)  # hydrosolutions time series forecast library.

# Get data.
# Discharge
qZ <- read.csv(file="data/stations/Zhamashike_runoff_00_14.csv", header=TRUE, sep=",")
qQilian <- read.csv(file="data/stations/Qilian_runoff_00_14.csv",header=TRUE,sep=",")
startDate <- c(2000,01,01)

# Default model parameters
error.test <- c("MAE", "R22", "RMSE")
ensembleSize <- 1
model.type <- 'mlpe'
test.for.fit <- 'MAE'
#load("./data/qQ.mlpe.fit.RData")
load("./data/qQ.mlpe.fit100.RData")

# Pre-processing for display of input data.
qZts <- ftimeseries:::readAndProcessData(qZ,startDate)
qQts <- ftimeseries:::readAndProcessData(qQilian,startDate)




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
    updateSelectInput(
      session, 
      "dischargeStation", 
      choices = c("Qilian",
                  "Zhamashike")
      #server = TRUE
    )
    output$timeSeriesDataText <- renderText({ 
      outtext = switch(input$data,
                       "Precipitation" = paste("Precipitation raster map."),
                       "Temperature" = paste("Temperature data."),
                       "Discharge" = paste("Discharge time series. The data of the selected station is drawn in red. Not selected data is drawn in grey."),
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
      plot(qZts[[1]], lty = 1, col = "gray", ylab = "Discharge [m3/s]",xlab = "Time [year]")
      lines(qQts[[1]], lty = 1, col = 'gray')
      #lines(qZMo, lty = 1, col = 'gray')
      stationName <- input$dischargeStation
      if (stationName != '') {
        variableName = 'qQts[[1]]'  # initialize
        variableName <- switch(stationName,
                               "Qilian" = 'qQts',
                               "Zhamashike" = 'qZts')
        otherLegendName <- switch(stationName,
                            "Qilian" = 'Zhamashike',
                            "Zhamashike" = 'Qilian')
        lines(get(variableName)[[1]],lty=1,col='red')
        legend("topleft",c(otherLegendName,stationName),lty=1,col=c('gray','red'))
      }
    }) 
    
    # Model configuration stuff.
    predictionQilian <- reactive({
      #ftimeseries:::predictDischargeWithDischarge(qQilian,startDate,input$ensembleSize,input$model.type,input$test.for.fit,error.test)
      ftimeseries:::predictDischargeWithDischarge2(qQilian,
                                                   startDate,
                                                   qQ.mlpe,
                                                   input$ensembleSize,
                                                   input$model.type,
                                                   input$test.for.fit,
                                                   error.test)
    })
    predictionZhamashike <- reactive({
      ftimeseries:::predictDischargeWithDischarge(qZ,startDate,input$ensembleSize,input$model.type,input$test.for.fit,error.test)
    })
    output$modelInfoText <- renderUI({
      str1 = switch(input$model.type,
                    'mlpe' = paste("The model of choice is the <b>multy-layer perceptron model</b>.", 
                                   "The multi-layer perceptron model is a feedforward artificial neural network model.",
                                   sep = "\n"))
      str2 = switch(input$test.for.fit,
                    "MAE" = paste("You chose <b>mean average error</b> as quality criteria."))
      str3 = paste("You chose an ensemble size of ",
                   "<b>",
                   input$ensembleSize,
                   "</b>",
                   ". That means that ",
                   input$ensembleSize,
                   " pre-fitted models are run in sequence. Your forecast will be the mean value from ",
                   input$ensembleSize,
                   " model runs. The uncertainty of the model prediction is estimated from these ",
                   input$ensembleSize,
                   " models.",
                   sep="")
      str4 = paste("The default tests for model error are: Mean average error (MAE), R22, and Root mean squared error (RMSE).")
      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
    })
    output$scatterPlotHeaderText <- renderText({
      outtext = paste("Scatter plot of the 5 years test set from June 2010 to December 2014. Shown are the mean values of the forecast ensemble and the spread of the ensemble.")
    })
    output$testSetTimeSeriesHeaderText <- renderUI({
      outtext1 = paste("Time series plot of the step-by-step prediction of the testing period.")
      outtext2 = paste("Observed (black) and predicted (blue) discharge at Qilian station from June 2010 to December 2014.")
      HTML(paste(outtext1,outtext2,sep='<br/>'))
    })
    output$scatterPlotQilian <- renderPlot({
      qQ           <- qQilian # input at top
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
        if((qQ.acf$acf[i] > 0.3)|(qQ.acf$acf[i] < -0.3)){
          correlation.of.data <-c(correlation.of.data,i)
        }
      }
      qQ.lag       <- CasesSeries(qQ,correlation.of.data)
      
      # Plot for one-step prediction ----
      predD        <- vector("list",1)
      predD$pred   <- qQp[[1]]
      err.mean     <- qQp[[2]]
      h            <- holdout(qQ.lag$y, 
                              ratio = 2/3, 
                              mode = "order", 
                              seed = 12345) # just for ID later on!
      ftimeseries:::ensScatterPlot(predD,
                                   qQ.lag,
                                   h,
                                   2,
                                   paste("Model errors: MAE", toString(round(err.mean[1], digits = 2)),
                                         ", R22", toString(round(err.mean[2], digits = 2)), 
                                         ", RMSE",toString(round(err.mean[3], digits = 2))),
                                   "observation [m3/s]",
                                   "forecast [m3/s]",
                                   input$ensembleSize)
      
    })
    output$testSetTimeSeries <- renderPlot({
      qQ           <- qQilian # input at top
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
      predD        <- vector("list",1)
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
    output$predictionTextQilian <- renderUI({
      qQ           <- qQilian # input at top
      qQ           <- qQ[,-1]
      qQ           <- as.vector(as.matrix(qQ))
      qQ           <- na.omit(qQ)
      start.date   <- startDate
      qQts         <- dwmTimeseries(qQ,start.date[1],start.date[2],start.date[3],"mean")
      qQ           <- qQts[[3]]
      
      qQp <- predictionQilian()
      qQpEns <- qQp[[1]]
      qQpTarget <- vector("list",1)
      qQpTarget[[1]] <- 0
      targetInput <- (as.numeric(input$target))
      for (i in 1:input$ensembleSize){
        qQpTarget[[1]] <- qQpTarget[[1]] + qQpEns[[i]][targetInput]
      }
      qQpTarget[[1]] <- qQpTarget[[1]]/input$ensembleSize
      
      qQpError <- qQp[[2]]
      monthString <- switch(input$target,
                            "1" = "June 2010",
                            "2" = "July 2010",
                            "3" = "August 2010",
                            "4" = "September 2010",
                            "5" = "October 2010",
                            "6" = "November 2010",
                            "7" = "December 2010",
                            "8" = "January 2011",
                            "9" = "February 2011",
                            "10" = "March 2011",
                            "11" = "April 2011",
                            "12" = "May 2011",
                            "13" = "June 2011",
                            "14" = "July 2011",
                            "15" = "August 2011",
                            "16" = "September 2011",
                            "17" = "October 2011",
                            "18" = "November 2011",
                            "19" = "December 2011",
                            "20" = "January 2012",
                            "21" = "February 2012",
                            "22" = "March 2012",
                            "23" = "April 2012",
                            "24" = "May 2012",
                            "25" = "June 2012",
                            "26" = "July 2012",
                            "27" = "August 2012",
                            "28" = "September 2012",
                            "29" = "October 2012",
                            "30" = "November 2012",
                            "31" = "December 2012",
                            "32" = "January 2013",
                            "33" = "February 2013",
                            "34" = "March 2013",
                            "35" = "April 2013",
                            "36" = "May 2013",
                            "37" = "June 2013",
                            "38" = "July 2013",
                            "39" = "August 2013",
                            "40" = "September 2013",
                            "41" = "October 2013",
                            "42" = "November 2013",
                            "43" = "December 2013",
                            "44" = "January 2014",
                            "45" = "February 2014",
                            "46" = "March 2014",
                            "47" = "April 2014",
                            "48" = "May 2014",
                            "49" = "June 2014",
                            "50" = "July 2014",
                            "51" = "August 2014",
                            "52" = "September 2014",
                            "53" = "October 2014",
                            "54" = "November 2014",
                            "55" = "December 2014"
                            )
      outtext1 = paste("The <b>predicted average montly discharge for Qilian</b> in",
                       "<b>",
                      monthString,
                      "</b>",
                      "is",
                      "<b>",
                      #toString(round(qQpEns[[1]][targetInput],digits=2)),
                      toString(round(qQpTarget[[1]],digits = 2)),
                      "m3/s</b> with an expected mean absolute error of",
                      toString(round(qQpError[[1]],digits = 2)),
                      "m3/s.",
                      sep=" ")
      outtext2 = paste("The figure below shows the time series of historical discharge values (black) until issue date (black circle) and the predicted time series (blue) until target date (blue circle).")
      HTML(paste(outtext1, outtext2, sep = '<br/><br/>'))
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
      qQ           <- qQilian # input at top
      qQ           <- qQ[,-1]
      qQ           <- as.vector(as.matrix(qQ))
      qQ           <- na.omit(qQ)
      start.date   <- startDate
      qQts         <- dwmTimeseries(qQ,start.date[1],start.date[2],start.date[3],"mean")
      qQ           <- qQts[[3]]
      qQp     <- predictionQilian()
      qQpEns   <- qQp[[1]]
      qQpErr   <- qQp[[2]]
      # Compute ensemble statistics.
      qQpTar <- vector("list",input$target)
      qQpStd <- vector("list",input$target)
      qQpMin <- vector("list",input$target)
      qQpMax <- vector("list",input$target)
      for (j in 1:input$target){
        qQpTar[[j]] <- 0
        qQpStd[[j]] <- 0
        qQpMin[[j]] <- 0
        qQpMax[[j]] <- 0
        # First compute ensemble mean.
        for (i in 1:input$ensembleSize){
          qQpTar[[j]] <- qQpTar[[j]] + qQpEns[[i]][j]
        }
        qQpTar[[j]] <- qQpTar[[j]]/input$ensembleSize
        # Then compute ensemble standard deviation.
        for (i in 1:input$ensembleSize){
          qQpStd[[j]] <- qQpStd[[j]] + (qQpEns[[i]][j] - qQpTar[[j]])^2
        }
        qQpStd[[j]] <- sqrt(qQpStd[[j]]/input$ensembleSize)
      }

      #plot(qQ, lty = 1, col = "black", ylab = "Discharge Qilian [m3/s]")
      # Plot the last 24 months of the historical time series in 'transparent' to get 
      # the plot extensions right.
      N <- 5  # Months cut off because of lag.
      L <- 120  # Index of time series of last day of training set. Here Dec. 2009.
      P <- 24  # Number of months of historical time series to display.
      
      x <- time(qQ)[L-P]
      y <- qQ[L-P]
      for (i in 1:P+N){
        x <- c(x,time(qQ)[L-P+i])
        y <- c(y,qQ[L-P+i])
      }
      for (i in 1:input$target){
        x <- c(x,(time(qQ)[L+N]+(time(qQ)[L+N]-time(qQ)[L-1+N])*i))
        y <- c(y,(qQpTar[[i]]+(qQpStd[[i]]*2)))
      }
      #par(las=2)
      #par(mar=c(8,8,1,1))
      plot(x,y, 
           type = "l", lty = 1, col = alpha("purple",0.0),  
           ylab = "Discharge Qilian [m3/s]")  #,xaxt="n")  # removes x-axis labels
      #axis(1, at=time(qQ), labels=as.Date(time(qQ)))
       
      # Draw a line in that plot for the historical time series until issue date in black.
      x <- time(qQ)[L-P]
      y <- qQ[L-P]
      for (i in 1:P){
        x <- c(x,time(qQ)[L-P+i])
        y <- c(y,qQ[L-P+i])
      }
      lines(x,y,lty=1,col='black')
      points(tail(x,n=1),tail(y,n=1),col='black')
      
      ## Draw a grey line for the 5 months the model cannot predict.
      #x <- time(qQ)[132]
      #y <- qQ[132]
      #for (i in 1:N+1){
      #  x <- c(x,time(qQ)[132+i])
      #  y <- c(y,qQ[132+i])
      #}
      #lines(x,y,lty=2,col='grey')
      
      ## Plot the range of plus-minus 2 standard deviations (computed from the ensemble).
      #x <- time(qQ)[L+1+N]
      #y <- qQpTar[[1]] + qQpStd[[1]] * 2
      #for (i in 2:input$target){
      #  x <- c(x,(time(qQ)[L+1+N]+(time(qQ)[L+1+N]-time(qQ)[L+N])*i))
      #  y <- c(y,(qQpTar[[i]]+(qQpStd[[i]]*2)))
      #}
      #lines(x,y,lty=1,col='gray',ylab = "Discharge Qilian [m3/s]")
      #y2 <- qQpTar[[1]] - qQpStd[[1]] * 2
      #for (i in 2:input$target){
      #  y2 <- c(y2,(qQpTar[[i]]-(qQpStd[[i]]*2)))
      #}
      #lines(x,y,lty=1,col='gray')
      ## Shade the area between the two gray lines
      #polygon(c(x,rev(x)),c(y2,rev(y)),col="gray",border=NA)
      
      # Plot each ensemble replicate in gray
      for (i in 1:input$ensembleSize){
        x <- time(qQ)[L+1+N]
        y <- qQpEns[[i]][1]
        for (j in 2:input$target){
          x <- c(x,(time(qQ)[L+1+N]+(time(qQ)[L+1+N]-time(qQ)[L+N])*j))
          y <- c(y,qQpEns[[i]][j])  
          lines(x,y,lty=1,col='gray')
        }
      }
      
      
      # Plot the ensemble mean of the prediction.
      x <- time(qQ)[L+1+N]
      for (i in 2:input$target){
        x <- c(x,(time(qQ)[L+1+N]+(time(qQ)[L+1+N]-time(qQ)[L+N])*i))
      }
      y <- qQpTar[[1]]
      for (i in 2:input$target){
        y <- c(y,(qQpTar[[i]]))
      }
      lines(x,y, lty = 1, col = 'blue')
      points(tail(x,n=1),tail(y,n=1),col='blue')

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
    