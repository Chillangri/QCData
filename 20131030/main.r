# ==========================================================================
# This program is developed to simuate YCC Quality Data using Control Chart.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Sep., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================

main <- function() {
  library("qcc")
  library("SixSigma")
  source("./getFileName.r")
  source("./loadData.r")
  
  dataFile <- getFileName()
  if(is.null(dataFile)==TRUE) on.exit()  # If working directory is not correct, this function will be terminated.
  rawData <- loadData(dataFile)
  
  #data(sourceData)
  #attach(sourceData)

  ################
  # User Input
  #
  firstCol  <- as.numeric(readline("Which is the first column to be analyzed? (Digit only): "))
  lastCol   <- as.numeric(readline("Which is the last column to be analyzed? (9999=last column): "))
    if (lastCol == 9999) lastCol <- ncol(rawData)  
  stdTarget <- as.numeric(readline("What is the targeted thickness of this product? "))
  lwrLimit  <- as.numeric(readline("What is the value of Lower Specification Limit(LSL)? "))
  uprLimit  <- as.numeric(readline("What is the value of Upper Specification Limit(USL)? "))
  
  ################
  # Make various datasets
  #
  sourceData <- rawData[,firstCol:lastCol]
  zoneData   <- sourceData - stdTarget
  maxCol <- ncol(sourceData)
  maxRow <- nrow(sourceData)
  sumData <- rep(0, maxCol)
  for (index in 1:maxCol) {
    sumData[index]  <- sum(zoneData[,index])
  }
  valCpk <- ss.ca.cpk(sumData, -33, 33)
  valZ   <- ss.ca.z(sumData, -33, 33)
  
  #sprintf("Cpk of the cumulated thickness of this product is %f\n and Six-sigam z value of this product is %f\n", valCpk, valZ)
  
  ################
  # Save results in PDF file
  #
  pdf(paste(getwd(), "/analysisResult", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep=""), paper="a4r", width = 9.7, bg="transparent")
  #dev.new()
  for (index in 1: maxRow) {
    if (index == 1) {
      plot(seq(maxCol), sourceData[1,], type = "l", ylim=(c(min(sourceData, na.rm=TRUE), max(sourceData, na.rm=TRUE))), col=index, ylab = "Individual thickness", xlab = "<----  width  ---->", main = "Individual thickness only")
    } else {
      lines(seq(maxCol), sourceData[index,], type="l", col=index)      
    }
  }
  par(new=F)
  for (index in 1: maxRow) {
    if (index == 1) {
      plot(seq(maxCol), sourceData[1,], type = "l", ylim=(c(min(sourceData, na.rm=TRUE), max(sourceData, na.rm=TRUE))), col=index, ylab = "Individual thickness", xlab = "<----  width  ---->", main = "Simulated thickness")
    } else {
      lines(seq(maxCol), sourceData[index,], type="l", col=index)      
    }
  }
  par(new=T)
  plot(seq(maxCol), sumData, type="l", col=1, lwd=5, axes=F, ylab="", xlab="" , main="")
  axis(side=4)
  mtext("Cumulated thickness", side=4, line=3, col=1)
  #lines(seq(maxCol), sumData, type="l", col=1,lwd=5)
  par(new=F)
  qcc(sumData, type="xbar.one", center = 0, nsigma = 6, title = "Simulated thickness", xlab = "<----  width  ---->", ylab = "Cummulated thickness (Target = 0)")
  qcc(sumData, type="xbar.one", title = paste("Simulated thickness: Cpk is ", format(valCpk, digits = 4), " and z is ", format(valZ, digits=4), ".", sep=""), xlab = "<----  width  ---->", ylab = "Cummulated thickness (Real value)")
  qcc(sourceData, type="xbar", title = "X-bar chart for a given dataset", xlab = "time comsuming --->")
  qcc(sourceData, type="S", title = "S chart for a given dataset", xlab = "time comsuming --->")
  qcc(sourceData, type="xbar", center = stdTarget, limits = c(lwrLimit, uprLimit), label.limits = c("LSL", "USL"), title = "X-bar chart using USL/LSL for a given dataset", xlab = "time comsuming --->")
  #qcc(sourceData, type="xbar", confidence.level=0.999997, title = "X-bar chart condidence level 99.9997% (time)")
  for (index in 1:maxCol) {
    qcc(sourceData[,index], type = "xbar.one", title = paste(index, "th column point of this product", sep=""), xlab = "time comsuming --->")
  }
  
  dev.off()
  
}