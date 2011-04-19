##################################################################################
# R-Script to read data from an xls-File
# Trying to enable the direct import from the xls-Files writen during the extraction
# of the volumes of the single acini.
##################################################################################

setwd("p:/doc/#R")
library(gdata)

data <- read.xls("TestData.xls", sheet="3")
data <- data[,2]
mean(data,na.rm=TRUE)
plot(data)
lines(data)
data.NumberOfAcini <-length(na.exclude(data))
cat("\tSheet", i, "contains", data.NumberOfAcini,"Acini")