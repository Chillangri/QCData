# ==========================================================================
# This is main program to simuating Oscillation Results for YCC.
#                                               Created by Dr. Jaeho H. BAE
#                                                       Assistant Professor
#                Dept. of Logistics & Distribution Mgmt. at Hyechon College
#                                                                Oct., 2013.
#                                                      chillangri@gmail.com
# ==========================================================================

oscSimul <- function() {
  library("qcc")
  source("./getFileName.r")
  source("./loadData.r")
  source("./funOscillation.r")
  source("./plotSumData.r")
  
  dataFile <- getFileName()
    if(is.null(dataFile)==TRUE) on.exit()  # If working directory is not correct, this function will be terminated.
  rawData <- loadData(dataFile)
  startRaw <- as.numeric(readline("Which is the first data column number of the file? "))
  rawData  <- rawData[,startRaw:(startRaw+399)]
  for(index in 1:400) {
    names(rawData)[index] <- paste("Col",index, sep="")
  }
  
  modeSelect    <- as.numeric(readline("Select Simulation Mode (1: Use default para., Other: Manual Input): "))
    if(modeSelect != 1) modeSelect <- 2
  
  ################
  # Parameters Setting.
  #
  if (modeSelect ==1) {                       # Use default parameters
    frameWidth   <-     8700
    noInspect    <-      400
    distInspect  <-       21
    lengthMR     <- 25547000
    timeMR       <-       87
    firstCol     <-       39
    lastCol      <-      360
    widthCol     <- lastCol - firstCol +1
    speedMA      <-  800000
    timeMA       <- lengthMR / speedMA
    speedOsc     <-    1000
    stdTarget    <-      40
    numInspCycle  <- length(rawData[,1])
  } else {                                    # Manual Input
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
    #10) Etc.
    stdTarget <- as.numeric(readline("What is the targeted thickness of this product? "))
    numInspCycle  <- length(rawData[,1])
  }
  
  ################
  # Display used parameters
  #  
  print("================================================================================================")
  print("||  Used Parameters")
  print("================================================================================================")
  print(paste("01. Full frame width: ", frameWidth, "(mm)", sep=""))
  print(paste("02. Total inspection points: ", noInspect, "(points)", sep=""))
  print(paste("03. Distance between inspection points: ", distInspect, "(mm)", sep=""))
  print(paste("04. Length of used MR prodcut: ", lengthMR, "(mm)", sep=""))
  print(paste("05. its working time in MR op.: ", timeMR, "(min)", sep=""))
  print(paste("06. Effective regions are: ", widthCol, "points, from ", firstCol, "th column to ", lastCol, "th column", sep=""))
  print(paste("07. Slitting speed in MA op.: ", speedMA, "(mm/min)", sep=""))
  print(paste("08. its expected time in MA op.: ", timeMA, "(min)", sep=""))
  print(paste("09. Oscillation speed: ", speedOsc, "(mm/min)", sep=""))
  print(paste("10. Standard target thickness of this product: ", stdTarget, "(micron)", sep=""))
  print(paste("11. Total number of inspection cycles: ", numInspCycle, "(times)", sep=""))
  print("================================================================================================")
  print("")
  
  ################
  # Fix simulation limits
  # 
  fromOsc     <- as.numeric(readline("Input lower simulation limit: (in mm)"))
  toOsc       <- as.numeric(readline("Input upper simulation limit: (in mm)"))
  incOsc      <- as.numeric(readline("Input incremental size: (in mm)"))
  
  scopeOsc    <- seq(fromOsc, toOsc, incOsc)
  simulatedSD <- rep(0, length(scopeOsc))
 
  # Set PDF file out parameter
  #  
  pdf(paste(getwd(), "/OscSimul", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep=""), paper="a4r", width = 9.2, bg="transparent")
  
  originalSD = plotSumData(rawData, firstCol, lastCol, widthCol, stdTarget)
  for (index in 1:length(scopeOsc)) {
    simulatedSD[index] = funOscillation(rawData, frameWidth, noInspect, distInspect, lengthMR, timeMR, firstCol, lastCol, widthCol, speedMA, timeMA, speedOsc, stdTarget, numInspCycle, scopeOsc[index])
    print(paste(index/length(scopeOsc)*100, "% completed...", sep=""))
  }
  par(new=F)
  plot(seq(length(scopeOsc)+1), c(originalSD, simulatedSD), type="b", ylab="Standard Deviation", xlab="Oscillation Width" , main=paste("SD Trend by oscillation from ", fromOsc, "mm  to ", toOsc, "mm by ", incOsc,"mm", sep=""), ylim=(c(min(simulatedSD), max(simulatedSD))))
  dev.off() 
}