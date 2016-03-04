require(ggplot2)
Data <- read.csv("/Users/habi/Dev/python/HealthData.csv",header=TRUE, sep=",")
names(Data)[names(Data)=="Steps..count."] <- "Steps"
stringDate <- as.character(Data$Start)
Data$Start <- as.Date(stringDate, "%d-%b-%Y %H:%M")
Data$Finish <- NULL
summary(Data$Steps)
p <- ggplot(Data, aes(Data$Start,Data$Steps)) + xlab('Datum') + ylab('Steps')
p + geom_point() + geom_smooth()