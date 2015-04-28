getFileName <- function() {
  #setwd("/Volumes/JHBae-Data/Dropbox/QCData")
  #setwd("/Volumes/JHBae-Data/Dropbox/QCData/SJ20131018")
  currentWD <- getwd()
  cat("Your DATA file must be at ", currentWD, "\n", sep="")
  ANS <- readline("Is it right? (Y/y or N/n): ")
  
  if ((substr(ANS, 1, 1)=="n") || (substr(ANS, 1, 1)=="N")) {
    return()
  } else {
    fileName <- readline("What is the file Name? ")
    fileName <- paste(currentWD, "/", fileName, sep="")
    return(fileName)
  }
}