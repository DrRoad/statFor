qZ <- read.csv(file="data/stations/Zhamashike_runoff_00_14.csv", header=TRUE, sep=",")
qQ <- read.csv(file="data/stations/Qilian_runoff_00_14.csv",header=TRUE,sep=",")
qY <- read.csv(file="data/stations/Yingluoxia_runoff_00_14.csv",header=TRUE,sep=",")
qZts <- dwmTimeseries(qZ,2000,01,01,sum)
qZDa=qZts[[1]];qZWe=qZts[[2]];qZMo=qZts[[3]];
qQts <- dwmTimeseries(qQ,2000,01,01,sum)
qQDa=qQts[[1]];qQWe=qQts[[2]];qQMo=qQts[[3]];
qYts <- dwmTimeseries(qY,2000,01,01,sum)
qYDa=qYts[[1]];qYWe=qYts[[2]];qYMo=qYts[[3]];

dischargeVarNamePartStation = list('qZ',col="blue")
dischargeVarNamePartTime = list('Da',lty=1)

Da = as.ts(qQDa,qYDa,qZDa)

plot(qQDa,col="blue")
lines(qYDa,col="darkorange")
lines(qZDa,col="forestgreen")

#plot.new()
#plot.window(xlim=c(1850,2020), ylim = c(0,5000000))
#axis(1)
#axis(2)
#title(main="Discharge")
#title(xlab="Time")
#title(ylab="Discharge [m3/s]")
#box()
plot(get(paste(dischargeVarNamePartStation[1],dischargeVarNamePartTime[1],sep="")),
      lty=dischargeVarNamePartTime$lty,
      col=dischargeVarNamePartStation$col,
      ann=FALSE)
box()
title(xlab="Time")
title(ylab="Discharge [m3/s]")
