##################################################################################
# R-Script to extract the statistics from the Acinar Volumes generated with
# MeVisLab. The data has been put into Excel and arranged into one large
# commaseparated file (AcinusExtractionDataFile.csv). After a first run with R, 
# I have deleted all the outliers (->AcinusExtractionDataFileWithoutOutliers.csv), 
# and plotted the data again (box- and beanplot)
##################################################################################
setwd("P:/doc/#R/AcinusPaper")
library(beanplot) # load Beanplot library (http://www.jstatsoft.org/v28/c01/)

# Read Data
Data.original <-read.csv("AcinusExtractionDataFile.csv",head=TRUE,sep=";")
# Draw original Data
boxplot(Data.original,
  main="Boxplot of Acinar Volumes, original data",
  xlab="Days after birth",
  notch=TRUE, # If the notches of two plots do not overlap this is "strong evidence" that the two medians differ
  varwidth=TRUE, # scale width of boxes to amount of samples
  col="lightgray"
  )
boxplot(Data.original,
  main="Boxplot of Acinar Volumes, original data, without drawing the outliers",
  xlab="Days after birth",
  notch=TRUE, # If the notches of two plots do not overlap this is "strong evidence" that the two medians differ
  varwidth=TRUE, # scale width of boxes to amount of samples
  col="lightgray",
  outline=FALSE)  
boxplot(Data.original[,1:2],
  main="Only Day 4 and 10, to see the difference",
  notch=TRUE,
  col="lightgray",
  outline=FALSE) # Plot only Day 4 and 10 to see the difference  
# beanplot(Data.original,
#   main="Beanplot of Acinar Volumes, original data",
#   col="lightgray")
summary(Data.original)

# Sorted AcinusExtractionDataFile in descending size order.
# Saved as AcinusExtractionDataFileWithoutOutliers.csv
# Iteratively used boxplo(Data[,column] to identified outliers, delete them, save file and plot again until no outliers are shown anymore.
# Repeated for each day, then you've got Data-File without outliers
Data<-read.csv("AcinusExtractionDataFileWithoutOutliers.csv",head=TRUE,sep=";")
# boxplot(Data[,5])
# Give out Details of Data
summary(Data)
# Calculate Mean and Standard Deviation
Data.mean <- mean(Data,na.rm=TRUE)
Data.sd <- sd(Data,na.rm=TRUE)
# Increase of Volume compared to Day 4
Data.mean/Data.mean[1]
# How many Volumes?

# Histogramme Plotten mit derselben x-Achse
# ---> evtl. für Bericht von Johannes
hist(Data$D4, plot=TRUE,breaks=10,xlim=c(0,0.3),main="Volume Histogram of Day 4",xlab="Volume")
hist(Data$D10,plot=TRUE,breaks=10,xlim=c(0,0.3),main="Volume Histogram of Day 10",xlab="Volume")
hist(Data$D21,plot=TRUE,breaks=10,xlim=c(0,0.3),main="Volume Histogram of Day 21",xlab="Volume")
hist(Data$D36,plot=TRUE,breaks=10,xlim=c(0,0.3),main="Volume Histogram of Day 36",xlab="Volume")
hist(Data$D60,plot=TRUE,breaks=10,xlim=c(0,0.3),main="Volume Histogram of Day 60",xlab="Volume")

# for(i in 1:5) N[,i]<-length(na.exclude(Data[,i]))
N04<-length(na.exclude(Data[,1]))
N10<-length(na.exclude(Data[,2]))
N21<-length(na.exclude(Data[,3]))
N36<-length(na.exclude(Data[,4]))
N60<-length(na.exclude(Data[,5]))
Data.NumberOfAcini <- c(N04,N10,N21,N36,60)
Data.NumberOfAcini

# Plot the Data
boxplot(Data,
  varwidth=TRUE,
  notch=TRUE,
  col="lightgray",
  main="Boxplot of Acinar Volumes without outliers",
  xlab="Days after birth"
  )
boxplot(Data[,1:2],
  varwidth=TRUE,
  main="Only Day 4 and 10",
  col="lightgray",
  notch=TRUE) # Plot only Day 4 and 10 to see the difference
boxplot(Data[,1:3],
  varwidth=TRUE,
  main="Days 4, 10 and 21",
  notch=TRUE,
  col="lightgray")
# beanplot(Data,
#   main="Beanplot of acinar volumes with removed outliers",
#   col="lightgray")

# Plot single days
par(mfrow=c(5,1))
par(mar=c(3,5,1,2)) # set margins (bottom, left, top, right)
plot(Data$D4)
plot(Data$D10)
plot(Data$D21)
plot(Data$D36)
plot(Data$D60)
par(mfrow=c(1,1))
par(mar=c(5,4,4,2)+0.1) # set margins back to default

# Display Mean an Standart Deviation in an xy-plot
days<-c(4,10,21,36,60)
plot(days,Data.mean,
  type="b", # both line and dots
  main="Means of acinar volumes",
  xlab="Days after birth",
  ylab="Mean acinar volume [microliter]")
points(days,Data.mean,col="red")
arrows(days, Data.mean - Data.sd, days, Data.mean + Data.sd,code = 3, col = "red") # Draw Arrows with +- SD

# Shift data to the right, if desired
boxplot(Data,
  varwidth=TRUE,
  notch=TRUE,
  col="lightgray",
  main="Boxplot of Acinar Volumes, with Means and Standard Deviation",
  xlab="Days after birth"
  )
Data.shifted <- seq(Data) + 0.2
points(Data.shifted, Data.mean, col = "red") # Draw Means into Boxplot
arrows(Data.shifted, Data.mean - Data.sd, Data.shifted, Data.mean + Data.sd,code = 3, col = "red") # Draw Arrows with +- SD

## save to TikZ-Graphics, ready for LaTeX
# require(tikzDevice)
# tikz('BoxPlotAcini.tex', standAlone = TRUE, width=5, height=5)
#   boxplot(Data,varwidth=TRUE,notch=TRUE,col="lightgray",main="Boxplot of Acinar Volumes",xlab="Days after birth")
#   Data.shifted <- seq(Data) + 0.25
#   points(Data.shifted, Data.mean, col = "red") # Draw Means into Boxplot
#   arrows(Data.shifted, Data.mean - Data.sd, Data.shifted, Data.mean + Data.sd,code = 3, col = "red") # Draw Arrows with +- SD
# dev.off()

## ANOVA (without knowing what I do...)
# fit <- aov(Data$D4 ~ Data$D10)
# plot(fit)

## T-Test (the same results as =ttest(d4,d10,2,3) in Excel)
# t.test(Data$D4,Data$D10)
# t.test(Data$D4,Data$D21)
# t.test(Data$D4,Data$D36)
# t.test(Data$D4,Data$D60)
# 
# t.test(Data$D10,Data$D21)
# t.test(Data$D10,Data$D36)
# t.test(Data$D10,Data$D60)
# 
# t.test(Data$D21,Data$D36)
# t.test(Data$D21,Data$D60)
# 
# t.test(Data$D36,Data$D60)