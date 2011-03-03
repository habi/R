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
  main="Boxplot of Acinar Volumes, including outliers, but not drawing them",
  xlab="Days after birth",
  notch=TRUE, # If the notches of two plots do not overlap this is "strong evidence" that the two medians differ
  varwidth=TRUE, # scale width of boxes to amount of samples
  col="lightgray",
  outline=FALSE)
beanplot(Data.original,
  main="Beanplot of Acinar Volumes, original data",
  col="lightgray")
summary(Data.original)

# Identified outliers, modified data (removed largest 25 acini of each day
# and saved as new .csv
Data<-read.csv("AcinusExtractionDataFileWithoutOutliers.csv",head=TRUE,sep=";")
# Give out Details of Data
summary(Data)
# Plot the Data
boxplot(Data,
  varwidth=TRUE,
  notch=TRUE,
  col="lightgray",
  main="Boxplot of Acinar Volumes with removed outliers",
  xlab="Days after birth"
  ) 
beanplot(Data,
  main="Beanplot of acinar volumes with removed outliers",
  col="lightgray")

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

# Calculate Mean and Standard Deviation
Data.mean <- mean(Data,na.rm=TRUE)
Data.sd <- sd(Data,na.rm=TRUE)

# Shift data to the right, if desired
boxplot(Data,
  varwidth=TRUE,
  notch=TRUE,
  col="lightgray",
  main="Boxplot of Acinar Volumes",
  xlab="Days after birth"
  )
Data.shifted <- seq(Data) + 0.25
par(mfrow=c(2,1))
par(mar=c(10, 5, 4, 2))
x <- 1:10
plot(x)
box("figure", lty="dashed")
par(mar=c(5, 5, 10, 2))
plot(x)
box("figure", lty="dashed")points(Data.shifted, Data.mean, col = "red") # Draw Means into Boxplot
arrows(Data.shifted, Data.mean - Data.sd, Data.shifted, Data.mean + Data.sd,code = 3, col = "red") # Draw Arrows with +- SD

## save to TikZ-Graphics, ready for LaTeX
# require(tikzDevice)
# tikz('BoxPlotAcini.tex', standAlone = TRUE, width=5, height=5)
# boxplot(Data)
# dev.off()

## ANOVA (without knowing what I do...)
# fit <- aov(Data$D4 ~ Data$D10)
# plot(fit)

## T-Test (the same results as =ttest(d4,d10,2,3) in Excel)
t.test(Data$D4,Data$D10)
t.test(Data$D4,Data$D21)
t.test(Data$D4,Data$D36)
t.test(Data$D4,Data$D60)

t.test(Data$D10,Data$D21)
t.test(Data$D10,Data$D36)
t.test(Data$D10,Data$D60)

t.test(Data$D21,Data$D36)
t.test(Data$D21,Data$D60)

t.test(Data$D36,Data$D60)
