##################################################################################
# R-Script to read data from an xls-Files containing the volume values of the
# extracted Acini. 
# First version: 19.04.2011
##################################################################################

setwd("p:/doc/#R/AcinusPaper")
library(gdata)

Day <- 10
Day <- sprintf("%02.0f", Day)
FileName <- paste("R108C",Day,".xls",sep="")
FileLocation <- paste("p:/doc/#Tables/AcinarTreeExtraction/",FileName,sep="")
Names <- sheetNames(FileLocation)

cat(FileName, "contains", sheetCount(FileLocation), "Sheets with the following names:\n")
for (i in 1:length(Names)) print(Names[i])

for (i in 2:length(Names)) {
  cat("reading Sheet #",i,"\n")
  Data <- read.xls(FileLocation,sheet=i) #read Sheet Nr. i
  Data <- Data[,2] # only keep Volumes (in the second column)
  Mean <- mean(Data,na.rm=TRUE) # Mean
  if (!is.nan(Mean)) {
    plot(Data,main=Names[i])
    lines(Data)
  }
  NumberOfAcini <-length(na.exclude(Data))
  cat("For", Names[i], "we counted", NumberOfAcini, "Acini with a mean volume of", Mean, "\n")
  }
  
