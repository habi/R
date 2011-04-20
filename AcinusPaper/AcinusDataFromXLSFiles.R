##################################################################################
# R-Script to read data from an xls-Files containing the volume values of the
# extracted Acini. 
# First version: 19.04.2011
##################################################################################

setwd("p:/doc/#R/AcinusPaper")
library(gdata)

Days <- c(04,10,21,36,60)
for (currentDay in 1:length(Days)) {
  Day <- Days[currentDay]
  Day
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
    RoundedMean <- sprintf("%.5f", Mean)
    NumberOfAcini <-length(na.exclude(Data))
    if (!is.nan(Mean)) {
      Title <- paste(Names[i],"|", NumberOfAcini, "Acini | Mean:", RoundedMean, "\n")
      plot(Data,type="b") 
      title(Title)
    }
    cat("For", Names[i], "we counted", NumberOfAcini, "Acini with a mean volume of", Mean, "\n")
    }
}