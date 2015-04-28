# ==========================================================================
# This program is developed to find theoretical CT for YCC.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Dec., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================

findCT <- function() {
#  library("qcc")
# library("cairoDevice")
  library("extrafont")
  source("./getFileName.r")
  source("./loadData.r")
# font_import() 
  
  dataFile <- getFileName()
    if(is.null(dataFile)==TRUE) on.exit()  # If working directory is not correct, this function will be terminated.
  rawData <- loadData(dataFile)
  
  nameData <- names(table(rawData$"Product"))
  sourceData <- as.list(nameData)
  saveDate   <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  fileName1 <- paste(getwd(), "/YCC01.DataSeg", saveDate, ".xlsx", sep="")
  lWB <- loadWorkbook(fileName1, create = TRUE)
  for(index in 1:length(nameData)) {
    sourceData[[index]] <- subset(x=rawData, subset=(rawData$"Product"==nameData[index]), select=c(Product, Class, WORKORDERNO, Class01, Class02, Throughput))
    createSheet(lWB, nameData[index])
    writeWorksheet(lWB, sourceData[[index]], nameData[index], header=TRUE)
  }
  saveWorkbook(lWB)
  
  
  fileName2 <- paste(getwd(), "/YCC02.Boxplot", saveDate, ".pdf", sep="")
  pdf(fileName2, paper="a4r", width = 9.7, bg="transparent")
  #if(Sys.info()[["sysname"]] == "Darwin") { 
  #      par(family="AppleGothic")
  #    } else {
  #     par(family="Korea1")
  #  }
  for(index in 1:length(nameData)) {
    par(mfrow=c(2,2))
    plot(1:length(sourceData[[index]]$"WORKORDERNO"), jitter(sourceData[[index]]$"Throughput"), main="", xlab="W/O No.", ylab="Throughput", xaxt="n")
    title(main=paste("Trend by W/O of ", nameData[index]), family="Korea1deb", font.main=1, col.main="blue")
      #axis.Date(1, at = seq(as.Date("2001/1/1"), max(as.Date(sourceData[[index]]$"Date"))+6, "months"))
      #axis.Date(1, at = seq(as.Date("2001/1/1"), max(as.Date(sourceData[[index]]$"Date"))+6, "weeks"),labels = FALSE, tcl = -0.2)
    boxplot(Throughput ~ Class, data=sourceData[[index]], main="Class vs. Throughput", xlab="Class", ylab="Throughput")
    boxplot(Throughput ~ Class01, data=sourceData[[index]], main="Class01 vs. Throughput", xlab="Class01", ylab="Throughput")
    boxplot(Throughput ~ Class02, data=sourceData[[index]], main="Class02 vs. Throughput", xlab="Class02", ylab="Throughput")
    par(new=F)
  }
  dev.off()
  
  fileName3    <- paste(getwd(), "/YCC03.ResultCT", saveDate, ".xlsx", sep="")
  sourceWB     <- loadWorkbook(fileName1)
  sourceWS     <- getSheets(sourceWB)
  resultData   <- data.frame(sq=1:length(nameData), Name=nameData, Max=0, min=0, Mean=0, Median=0, SD=0, "Sigam*3"=0, "Sigma*5"=0, TH1=0, TH2=0, CT1=0, CT2=0, "비고1"=0, "비고2"=0)
  
  for (wbIndex in 1:length(nameData)) {
    wsData     <- readWorksheet(sourceWB, sheet=nameData[wbIndex])
    if(length(wsData[,6]) <8) {
      wsMax      <- max(wsData[,6])
      wsMin      <- min(wsData[,6])
      wsMean     <- mean(wsData[,6])
      wsMedian   <- median(wsData[,6])
      #wsSD       <- sd(wsData[,6])
      resultData[wbIndex, 3] <- wsMax
      resultData[wbIndex, 4] <- wsMin
      resultData[wbIndex, 5] <- wsMean
      resultData[wbIndex, 6] <- wsMedian
      #resultData[wbIndex, 7] <- wsSD
      #resultData[wbIndex, 8] <- 3*wsSD
      #resultData[wbIndex, 9] <- max(wsMean, wsMedian) + 3*wsSD
      #resultData[wbIndex,10] <- 1/(max(wsMean, wsMedian) + 3*wsSD)
      resultData[wbIndex,14] <- length(wsData[,6])
      resultData[wbIndex,15] <- wsMax*1.2
    } else {
      wsMax      <- max(wsData[,6])
      wsMin      <- min(wsData[,6])
      wsMean     <- mean(wsData[,6])
      wsMedian   <- median(wsData[,6])
      wsSD       <- sd(wsData[,6])
      resultData[wbIndex, 3] <- wsMax
      resultData[wbIndex, 4] <- wsMin
      resultData[wbIndex, 5] <- wsMean
      resultData[wbIndex, 6] <- wsMedian
      resultData[wbIndex, 7] <- wsSD
      resultData[wbIndex, 8] <- 3*wsSD
      resultData[wbIndex, 9] <- 5*wsSD
      resultData[wbIndex,10] <- wsMean + 3*wsSD
      resultData[wbIndex,11] <- wsMean + 5*wsSD
      resultData[wbIndex,12] <- 1/(wsMean + 3*wsSD)
      resultData[wbIndex,13] <- 1/(wsMean + 5*wsSD)
      resultData[wbIndex,14] <- wsMean + 3*wsSD - wsMax
      resultData[wbIndex,15] <- wsMean + 5*wsSD - wsMax
    }
  }
  xlcFreeMemory()
  
  resultWB <- loadWorkbook(fileName3, create = TRUE)
  resultWS <- "Calculated CT"
  for(index in 1:length(nameData)) {
    createSheet(resultWB, resultWS)
    writeWorksheet(resultWB, resultData, resultWS, header=TRUE)
  }
  saveWorkbook(resultWB)  
  
#  sink(fileName)
#  sink()
#  unlink(fileNam)
  
  
  
}