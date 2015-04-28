setwd("/Volumes/JHBae-Data/Dropbox/QCData")
library("qcc")
source("getFileName.r")
source("loadData.r")
dataFile <- getFileName()
# What is the file Name? RegressionModel.xlsx
rawData <- loadData(dataFile)
regData <- rawData
linearReg <- lm(regData$Col.125 ~ regData$MDL05, regData)
summary(linearReg)
plot(linearReg)


mLinearReg <- lm(regData$Col.125 ~ regData$MDM01 + regData$MDM02 + regData$MDM03, regData)
summary(mLinearReg)
plot(mLinearReg)

library(Hmisc)
rcorr(as.matrix(regData[,2:7]), type="spearman")

pdf("Corelations Analysis.pdf", paper="a4r", width=9.7)
qcc(regData$Col.125, type="xbar.one")
qcc(regData$MDL05, type="xbar.one")
qcc(regData$MDM01, type="xbar.one")
qcc(regData$MDM02, type="xbar.one")
qcc(regData$MDM03, type="xbar.one")
qcc(regData$MDH01, type="xbar.one")
plot(linearReg)
plot(mLinearReg)
dev.off()


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
mydata <- rawData[, 2:7]
fit <- princomp(mydata, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(mydata, nfactors=5, rotate="varimax")
fit # print results

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(mydata, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names

http://www.statmethods.net/advstats/factor.html
http://www.6025.co.kr/statistics/yo.asp
