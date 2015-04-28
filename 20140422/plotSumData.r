# ==========================================================================
# This program is developed to simuate Oscillation Results for YCC.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Oct., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================
plotSumData <- function(rawData, firstCol, lastCol, widthCol, stdTarget) {

  sourceData    <- rawData[, firstCol:lastCol]

  sumData <- rep(0, widthCol)
  for (index in 1:widthCol) {
    sumData[index]  <- sum(sourceData[,index]-stdTarget)
  }
  
  par(new=F)
  for (index in 1: widthCol) {
    if (index == 1) {
      plot(seq(widthCol), sourceData[1,], type = "l", ylim=(c(min(sourceData), max(sourceData))), col=index, ylab = "Thickness", xlab = "<----  width  ---->", main = paste("Original thickness simulation result: its SD is", sd(sumData)))
    } else {
      lines(seq(widthCol), sourceData[index,], type="l", col=index)      
    }
  }
  par(new=T)
  plot(seq(widthCol), sumData, type="l", col=1, lwd=5, axes=F, ylab="", xlab="" , main="", ylim=(c(min(sumData), max(sumData))))
  axis(side=4)
  mtext("Cumulated thickness (Original Data)", side=4, line=3, col=1)
return(sd(sumData))
}