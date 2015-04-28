# ==========================================================================
# This program is developed to simuate Oscillation Results for YCC.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Oct., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================

oscillation <- function() {
  library("qcc")
  source("./getFileName.r")
  source("./loadData.r")
  
  dataFile <- getFileName()
    if(is.null(dataFile)==TRUE) on.exit()  # If working directory is not correct, this function will be terminated.
  rawData <- loadData(dataFile)
  startRaw <- as.numeric(readline("Which is the first data column number of the file? "))
  rawData  <- rawData[,startRaw:(startRaw+399)]
  for(index in 1:400) {
    names(rawData)[index] <- paste("Col",index, sep="")
  }
  
  ################
  # User Input
  #
  # 1) Thickness Inspector Frame Width
  frameWidth    <- as.numeric(readline("Input frame Width in mm scale (0 = 8700 mm): "))
    if(frameWidth == 0) frameWidth <- 8700
  # 2) Number of Inspection Points
  noInspect <- as.numeric(readline("Input number of inspection points (0 = 400 points): "))
    if(noInspect == 0) noInspect <- 400  
  # 3) Distance between Inspection Points
  distInspect   <- as.numeric(readline("Input distance between inspection points in mm scale (0 = 21 mm): "))
    if(distInspect == 0) distInspect <- 21  
  # 4) Total length of MR product
  lengthMR      <- as.numeric(readline("Input total length of MR Product for analysis(in m): "))
    if(lengthMR < 5000) lengthMR <- 21427
    lengthMR <- lengthMR * 1000
  # 5) Total time for manufacturing MR product
  timeMR        <- as.numeric(readline("Input total processing time for manufacturing an MR product(in miniute): "))
    if(timeMR < 30) timeMR <- 91
  # 6) Effective width of an MR product
#   regionProduct <- as.numeric(readline("Input width of effective product region(in mm): "))
  # 7) Starting/last inspection point and its width in points
  firstCol      <- as.numeric(readline("Input starting inspection point of effective product region: "))
  lastCol       <- as.numeric(readline("Input last inspection point of effective product region: "))
  widthCol      <- lastCol - firstCol + 1
  # 8) Parameters of main slitting
  #timeMA        <- as.numeric(readline("Input total main slitting time for manufacturing an MR product(in miniute): "))
  #  if(timeMR ==0) timeMA <- 91
  speedMA        <- as.numeric(readline("Input average speed of main slitting for an MR product(in m/min): "))
    if(speedMA == 0) speedMA <- 800
  speedMA        <- speedMA * 1000
  timeMA         <- lengthMR / speedMA
  # 9) Oscillation parameters
  speedOsc      <- as.numeric(readline("Input oscillation speed in frame direction(in m/min): "))
  speedOsc      <- speedOsc * 1000
  widthOsc      <- as.numeric(readline("Input oscillation width(in mm): "))
  #10) Etc.
  stdTarget <- as.numeric(readline("What is the targeted thickness of this product? "))
  numInspCycle  <- length(rawData[,1])
  
  ################
  # Parameter Calulation
  #  
  sourceData    <- rawData[, firstCol:lastCol]
  timeTau       <- timeMA / numInspCycle
  noSourceDataCol <- length(sourceData[1,])
 
  ################
  # Oscillation Simulation
  #  
  oscData       <- rawData 
  
  for(indexY in 2:numInspCycle) {
    #indexY      <- indexY + 1
    tactTau     <- timeTau * (indexY - 1)
    flagIndex   <- (-1)^(floor((speedOsc*tactTau)/widthOsc))
    moveX       <- floor(((speedOsc * tactTau) - floor((speedOsc*tactTau)/widthOsc)*widthOsc) / distInspect)
    deltaX      <- (speedOsc * tactTau) - floor((speedOsc*tactTau)/widthOsc)*widthOsc - (moveX * distInspect)
    delta       <- abs(deltaX) / distInspect
    print(paste("Row:", indexY, ", flagIndex:", flagIndex, ", moveX: ", moveX, ", deltaX: ", deltaX, ", delta: ", delta))

    if (flagIndex == 1) {
      for(indexI in 1:noInspect) {
        if (indexI >= (noInspect-moveX)) {  
          oscData[indexY, indexI] <- 0
        } else {
          deltaY                  <- (rawData[indexY, indexI + moveX + 1] - rawData[indexY, indexI + moveX]) * delta
          oscData[indexY, indexI] <- rawData[indexY, indexI + moveX] + deltaY
        }
      } 
    } else {
      for(indexI in noInspect:1) {
        if(indexI <= (moveX +1) ) {
          oscData[indexY, indexI] <- 0
        } else {
          deltaY                  <- (rawData[indexY, indexI - moveX] - rawData[indexY, indexI -moveX -1]) * delta
          oscData[indexY, indexI] <- rawData[indexY, indexI - moveX] + deltaY
        }
      }
    }     
  }

  print(head(rawData-oscData))
  oscSourceData    <- oscData[, firstCol:lastCol]
  
  ################
  # Oscillation Result Display
  # 
  sumData <- rep(0, noSourceDataCol)
  oscSumData <- rep(0, noSourceDataCol)
  for (index in 1:noSourceDataCol) {
    sumData[index]  <- sum(sourceData[,index]-stdTarget)
    oscSumData[index] <- sum(oscSourceData[,index]-stdTarget)
  }
  
  print(sumData-oscSumData)
 ################
  # Save results in PDF file
  #
  pdf(paste(getwd(), "/OscResult", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep=""), paper="a4r", width = 9.2, bg="transparent")
  #Original Data
  for (index in 1: noSourceDataCol) {
    if (index == 1) {
      plot(seq(noSourceDataCol), sourceData[1,], type = "l", ylim=(c(min(sourceData), max(sourceData))), col=index, ylab = "Thickness", xlab = "<----  width  ---->", main = paste("Original thickness simulation result: its SD is", sd(sumData)))
    } else {
      lines(seq(noSourceDataCol), sourceData[index,], type="l", col=index)      
    }
  }
  par(new=T)
  plot(seq(noSourceDataCol), sumData, type="l", col=1, lwd=5, axes=F, ylab="", xlab="" , main="", ylim=(c(min(sumData), max(sumData))))
  axis(side=4)
  mtext("Cumulated thickness (Original Data)", side=4, line=3, col=1)
  
  #Oscillated Data
  par(new=F)
  for (index in 1: noSourceDataCol) {
    if (index == 1) {
      plot(seq(noSourceDataCol), oscSourceData[1,], type = "l", ylim=(c(min(sourceData), max(sourceData))), col=index, ylab = "Thickness", xlab = "<----  width  ---->", main = paste("Oscillated thickness simulation result: oscWidth ", widthOsc, " and it SD is ", sd(oscSumData)))
    } else {
      lines(seq(noSourceDataCol), oscSourceData[index,], type="l", col=index)      
    }
  }
  par(new=T)
  plot(seq(noSourceDataCol), oscSumData, type="l", col=1, lwd=5, axes=F, ylab="", xlab="" , main="", ylim=(c(min(sumData), max(sumData))))
  axis(side=4)
  mtext("Cumulated thickness (Osillated)", side=4, line=3, col=1)
  par(new=F)
  
  dev.off()
}