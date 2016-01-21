# server.R

rm(list = ls()) # Clear Workspace 
cat("\014")
graphics.off() 
library(devtools) # Load Packages
library(forecast) 
library(rminer)
library(R.matlab) 
library(scales)
library(timeSeries) 
library(ggplot2)
library(plyr)
library(zoo) 
library(xts)
library(matrixStats) 
library(dplyr) 
library(numDeriv) 
library(astsa) 
library(tsDyn)
library(raster) 
library(rgdal)
library(ggmap) 
library(hddtools)
library(RNetCDF)
library(rts)
library(rgeos)
library(ncdf4)

# Get data.
# Discharge
qZ <- read.csv(file="data/Zhamashike_runoff.csv", header=TRUE, sep=",")
qQ <- read.csv(file="data/Qilian_runoff.csv", header=TRUE, sep=",")
qY <- read.csv(file="data/Yingluoxia_runoff.csv", header=TRUE, sep=",")
qZ <- qZ[,-1]
qZ <- as.vector(as.matrix(qZ))
qZ <- na.omit(qZ)
qQ <- qQ[,-1]
qQ <- as.vector(as.matrix(qQ))
qQ <- na.omit(qQ)
qY <- qY[,-1]
qY <- as.vector(as.matrix(qY))
qY <- na.omit(qY)
# Snow
sc.Q <- read.csv(file="data/JulesQilian.csv", header=TRUE, sep=",")
sc.Z <- read.csv(file="data/JulesZhamashike.csv", header=TRUE, sep=",")


shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      outtext = switch(input$data,
                       "Topography" = paste("Topographical map of the catchment area."),
                       "Precipitation" = paste("Precipitation raster map."),
                       "Temperature" = paste("Temperature data."),
                       "Discharge" = paste("Discharge time series."),
                       "Snow cover" = paste("Snow cover data."))
    })
    
    # Send a pre-rendered image, and don't delete the image after sending it
    output$preImage <- renderImage({
      # 
      filename <- switch(input$data,
                         "Topography" = list("topo.pdf"),
                         "Precipitation" = list("topo.pdf"),
                         "Temperature" = list("topo.pdf"),
                         "Discharge" = list("topo.pdf"),
                         "Snow cover" = list("topo.pdf")
                         )
      
      filename <- normalizePath(file.path('./images',filename))
      
      # Return a list containing the filename and alt text
      list(src = filename,
           alt = paste("", input$data))
      
    }, deleteFile = FALSE)
      
  }
)
    