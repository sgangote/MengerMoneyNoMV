#set wd of this script
#setwd("/Users/Santi/Desktop/ABM/MengMonNetLogo/NoMV/")
#source('DataAnalysisNoMV.r')

#set NOAWT to 1 to run  
Sys.setenv(NOAWT=1)

setwd("/Applications/NetLogo 5.1.0/")
#load RNetLogo package
library("RNetLogo")
#specify path to NetLogo.jar
nl.path <- getwd()
#Start NetLogo session with GUI (only from JGR)
NLStart(nl.path)

#Start Netlogo session without GUI
#NLStart(nl.path, gui = FALSE)

#Load Model first set path to model then load the model
model.path<-"/Users/Santi/Desktop/ABM/MengMonNetLogo/NoMV/MengMonNoMV.nlogo/"
NLLoadModel(model.path)


#Load useful functions for RNetLogo
source("/Users/Santi/Desktop/ABM/RNetlogoResources/RNetLogoFunctions.r")

population<-300
NLCommand(paste("set population",population))
numOfGoods<-3

NLCommand(paste("set num-of-goods",numOfGoods))
NLCommand("setup")

aggMoeFreq<-NLDoReport(50, "go", 
						c("ticks", paste("item ",0:(numOfGoods - 1)," agg-moe-strategy-good",sep="")), 
						as.data.frame = TRUE, 
						df.col.names = c("tick", paste("good",1:numOfGoods, sep="")))
#convert absolute to frequencies
aggMoeFreq[,c(1:numOfGoods + 1)]<-aggMoeFreq[,c(1:numOfGoods + 1)] / population


#boxplot freq of goods
boxplot(aggMoeFreq[,c(2:(numOfGoods + 1))])
