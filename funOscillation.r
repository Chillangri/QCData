# ==========================================================================
# This program is developed to simuate Oscillation Results for YCC.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Oct., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================

funOscillation <- function(rawData, frameWidth, noInspect, distInspect, lengthMR, timeMR, firstCol, lastCol, widthCol, speedMA, timeMA, speedOsc, stdTarget, numInspCycle, widthOsc) {
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
#    print(paste("Row:", indexY, ", flagIndex:", flagIndex, ", moveX: ", moveX, ", deltaX: ", deltaX, ", delta: ", delta))

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

 # print(head(rawData-oscData))
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
  
#  print(sumData-oscSumData)
 ################
  # Save results in PDF file
  #
#  pdf(paste(getwd(), "/OscResult", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep=""), paper="a4r", width = 9.2, bg="transparent")

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

return(sd(oscSumData))

}