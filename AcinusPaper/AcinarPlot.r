##################################################################################
# R-Script to extract the statistics from the Acinar Volumes generated with
# MeVisLab. The data has been put into Excel and arranged into one large
# commaseparated file (AcinusExtractionDataFile.csv). After a first run with R, 
# I have deleted all the outliers (->AcinusExtractionDataFileWithoutOutliers.csv)
#
#
#
#
##################################################################################
setwd("P:/doc/#R/AcinusPaper")
days <- c(4,10,21,36,60)

# Read Data
Data.original <-read.csv("AcinusExtractionDataFile.csv",head=TRUE,sep=";")
# Draw original Data
boxplot(Data.original,
  main="Boxplot of Acinar Volumes, with outliers"
  )
# Draw original Data without outliers
boxplot(Data.original,
  main="Boxplot of Acinar Volumes, without outliers",
  outline=FALSE
  )

# Identified outliers, modified data, saved as new .csv
# Plot it again, with notches and nice color
Data<-read.csv("AcinusExtractionDataFileWithoutOutliers.csv",head=TRUE,sep=";")
boxplot(Data,
  varwidth=TRUE, # scale width of boxes to amount of samples
  notch=TRUE, # If the notches of two plots do not overlap this is "strong evidence" that the two medians differ
  col="lightgray",
  main="Boxplot of Acinar Volumes",
  xlab="Days") 
	
summary(Data)
Data.means <- mean(Data,na.rm=TRUE)
Data.sd <- sd(Data,na.rm=TRUE)

plot(days,Data.sd)
  
quantile(Data$D4,na.rm=TRUE)
quantile(Data$D10,na.rm=TRUE)
quantile(Data$D21,na.rm=TRUE)
quantile(Data$D36,na.rm=TRUE)
quantile(Data$D60,na.rm=TRUE)

## save to TikZ-Graphics, ready for LaTeX
# require(tikzDevice)
# tikz('BoxPlotAcini.tex', standAlone = TRUE, width=5, height=5)
# boxplot(Data)
# devv.off()

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
