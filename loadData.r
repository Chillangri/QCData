loadData <- function(dataFile) {
  options(java.parameters = "-Xmx4096m")
  library(XLConnect)
    
  dataWB   <- loadWorkbook(dataFile)
  dataWS   <- getSheets(dataWB)
  
  cntWS    <- 0
  for(idx in dataWS) cntWS = cntWS+1
  
  if(cntWS==1){
    sourceData <- readWorksheet(dataWB, sheet=1)
    
  } else {
    cat("All Sheets on this file are followings:\n", dataWS)
    numSelect <- readline("Which Worksheet was made for Analysis? (number) ")
    numSelect <- as.integer(numSelect)
    sourceData <- readWorksheet(dataWB, sheet=dataWS[numSelect])
  }
  xlcFreeMemory()
  return(sourceData)
  
}