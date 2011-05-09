##################################################################################
# R-Script to read data from an xls-Files containing the volume values of the
# extracted Acini. 
# 19.04.2011: First version
# 20.04.2011: Looping through all Days, Integrating Boxplots
# 21.04.2011: Finally got the Array-thingy working.
##################################################################################

rm(list=ls()) # clear EVERYTHING!

setwd("p:/doc/#R/AcinusPaper")
library(gdata) # needed to read XLS-Files (needs an installation of PERL: http://www.perl.org/get.html)
library(outliers)
require(tikzDevice)

## Flags for Execution
DoPlots=0       # 1 does the Plots, something else doen't
DoBoxplots=1    # 1 does the BoxPlots, something else doen't
DivideThroughSize = 0 # Divide the acinar volumes through the size of the RUL from Datenblattstefan.xls
PrintToTikZ = 0 # Save Final BoxPlot in TeX-Output

## Initialize Variables
Time <- format(Sys.time(), "%d.%b %H:%M")
Days <- c(04,10,21,36,60)
Mean = array(NaN,c(5,5))
Median = array(NaN,c(5,5))
GlobalMean <- NaN
NumberOfCounts <- NaN
NumberOfAcini <- NaN
ConcatenatedVolumes = array(NaN,c(200,5,5)) # Initialize Array for Volumes (200 counts, 5 Animals per Day, 5 Days)
TotalVolumes = array(NaN,c(1000,length(Days))) # Initialize (huge and empty) Array for TotalVolumes so we can fill it later (1000=200x5)

# Days <- 04
# Days <- 10
# Days <- 21
# Days <- 36
# Days <- 60

## Read RUL-VOlumes from Stefans XLS-File to calculate with them afterwards.
cat("Reading Volumes from 'Datenblattstefan.xls'\n")
StefansVolumes = array(NaN,c(5,5),
    dimnames = list(c(1,2,3,4,5),c("4", "10", "21", "36", "60"))) # Initializse empty array to save values into.
StefansParenchyma = array(NaN,c(5,5),
    dimnames = list(c(1,2,3,4,5),c("4", "10", "21", "36", "60"))) # Initializse empty array to save values into.    
SheetNamesStefan <- sheetNames("Datenblattstefan.xls") # Read in Sheetnames of Datenblattstefan.xls
for (currentDay in 1:length(Days)) {
    TMP <- read.xls("Datenblattstefan.xls", sheet=currentDay+1) # Since Stefan has an "All Groups" sheet as first one, we read from the second one on.
    StefansVolumes[,currentDay]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]]) # read in as Factor, according to http://is.gd/QHQpCH, this makes values numeric.
    StefansParenchyma[,currentDay]<-as.numeric(levels(TMP$X.10[11:15])[TMP$X.10[11:15]]) # read in as Factor, according to http://is.gd/QHQpCH, this makes values numeric.
    cat("Reading RUL-Volumes & Parenchyma-Fraction in 'Datenblattstefan.xls':",SheetNamesStefan[currentDay+1],"to 'StefansVolumes', Column",currentDay,"\n")
}

## Loop through data in Days construct FileName for XLS-File
for (currentDay in 1:length(Days)) { # Iterate trough the days
    ## Construct Day, FileName and read/print relevant Info to console
    cat("---\n")
    Day <- sprintf("%02.0f", Days[currentDay]) # Construct and format the 'Day' nicely, so we can read the Filenames correctly
    cat("Working on Data from Day", Day, "\n")
    FileName <- paste("R108C",Day,".xls",sep="")
    FileLocation <- paste("p:/doc/#Tables/AcinarTreeExtraction/",FileName,sep="")
    SheetNames <- sheetNames(FileLocation) # Extract Names of the single Sheets in the XLS-Files
    cat("---\n")
    ## In a first loop, read out all the sheets in the file and plot the volume data
    cat("Extracting single Volumes\n")        
    for (currentSheet in 1:length(SheetNames)) { # Iterate through every sheet in the current XLS-File
#         cat("reading Sheet #",currentSheet," named '",SheetNames[currentSheet],"'\n",sep="")
        Data <- read.xls(FileLocation,sheet=currentSheet) # read Sheet Nr. "currentSheet"
        Mean[currentSheet,currentDay] <- mean(Data$Volume,na.rm=TRUE) # Calculate the arithmetic mean without the empty cells and save into current Mean
        Median[currentSheet,currentDay] <- median(Data$Volume,na.rm=TRUE) # Calculate the median without the empty cells and save into current Median
        NumberOfCounts[currentSheet] <- length(Data$Volume)
        NumberOfAcini[currentSheet] <- length(na.exclude(Data$Volume))
        if (!is.nan(Mean[currentSheet,currentDay])) { # Plot the Data for Acini where we actually have Data (i.e., where Mean[currentSheet,currentDay] is not empty)
            if (DoPlots==1) {
                plot(Data$Volume,type="b",col="red")
                PlotTitle <- paste(
                    SheetNames[currentSheet],"|",
                    NumberOfAcini[currentSheet],
                    "Acini | Mean:",sprintf("%.4f", Mean[currentSheet,currentDay]), "\n")
                abline(h=Mean[currentSheet,currentDay], col = "gray60") # Plot Mean[currentSheet,currentDay]
                title(PlotTitle)
            }
            cat("For", SheetNames[currentSheet], "we counted",
                NumberOfAcini[currentSheet], "Acini with a mean volume of",
                Mean[currentSheet,currentDay], "(and omitted",sum(is.na(Data$Volume)),"empty counts).\n") # Give out something to read in the console
        } # end if Mean[currentSheet,currentDay] is not empty
        if (is.nan(Mean[currentSheet,currentDay])) { # if Mean[currentSheet,currentDay] is empty, we probably have an empty Sheet...
            cat("For", SheetNames[currentSheet], "we counted no Acini, which means that the sheet is probably emtpy.\n")
        } # end if Mean[currentSheet,currentDay] *is* empty
    } # end iterate through sheets

    cat("---\n")    
    ## In a second loop below, read out all the data again (could probably be done in the same loop), concatenate into an Array and use this for a Boxplot
    if (DoBoxplots==1) {
        cat("Extracting single Volumes and saving them into an array for boxplotting\n")
        for (currentSheet in 1:length(SheetNames)) { # Iterate through every sheet in the current XLS-File
            cat("Working on sheet ",SheetNames[currentSheet]," (",currentSheet,"/",length(SheetNames),")\n",sep="")
            Data <- read.xls(FileLocation,sheet=currentSheet) # read Sheet Nr. i
            ConcatenatedVolumes[1:length(Data$Volume),currentSheet,currentDay] = Data$Volume # put just read Data into ith column of Array
            # Divide through the size of the single RUL, as specified by Stefan
            if (DivideThroughSize == 1) {
                cat("-------------------------------------------------------------------------\n")
                cat("Dividing ConcatenatedVolumes[,",currentSheet,currentDay,"]",
                    "through StefanStefansVolumes[",currentSheet,",",currentDay,"]\n",sep="")
                ConcatenatedVolumes[,currentSheet,currentDay]=ConcatenatedVolumes[,currentSheet,currentDay]/StefansVolumes[currentSheet,currentDay]
                Mean[currentSheet,currentDay] = Mean[currentSheet,currentDay]/StefansVolumes[currentSheet,currentDay]
                Median[currentSheet,currentDay] = Median[currentSheet,currentDay]/StefansVolumes[currentSheet,currentDay]
                cat("-------------------------------------------------------------------------\n")
            } # endif DivideThroughSize
            cat("Day:",currentDay,
                "|Sheet:",currentSheet,
                "|Our mean volume:",Mean[currentSheet,currentDay],
                "|Stefans Volume:",StefansVolumes[currentSheet,currentDay],"\n",sep="")
            TotalVolumes[1:length(c(ConcatenatedVolumes[,,currentDay])),currentDay] <- c(ConcatenatedVolumes[,,currentDay])
            GlobalMean[currentDay] = mean(TotalVolumes[,currentDay],na.rm=TRUE)
        } # end iterate through sheets
        BoxPlotTitle <- paste(FileName,
            "| total Acini:", sum(NumberOfAcini),
            "| global Mean:", sprintf("%.4f", mean(Mean[,currentDay],na.rm=TRUE)),"\n")
        boxplot(ConcatenatedVolumes[,,currentDay],
            notch=TRUE,
            varwidth=TRUE,
            col="lightgray",
            outline=TRUE, # Plot plot outliers
            names=SheetNames,
            main=BoxPlotTitle)
        abline(h=GlobalMean[currentDay], col = "green")
        points(c(1:5),Mean[,currentDay],col="red") # Plot Means
        text(c(1:5),Mean[,currentDay],sprintf("%.4f",Mean[,currentDay]), pos=4, col="red") # Print Means in Plots, at the Position of the Means!
        points(c(1:5)+.33,Median[,currentDay],col="blue") # Plot Medians
        text(c(1:5)+.33,Median[,currentDay],sprintf("%.4f",Median[,currentDay]),cex=1, pos=4, col="blue") # Print Medians in Plots, at the Position of the Medians!
        cat("---\n")
    } # endif DoBoxplots
} # end iterate through Days

# Plot Increase
Increase <- GlobalMean/GlobalMean[1]

# Stefans Data
StefansMeanVolumes <- c(
    mean(StefansVolumes[,1]),
    mean(StefansVolumes[,2]),
    mean(StefansVolumes[,3]),
    mean(StefansVolumes[,4]),
    mean(StefansVolumes[,5])) # RUL from Datenblattstefan.xls
StefansIncrease <- StefansMeanVolumes / StefansMeanVolumes[1] # from p:\doc\#R\AcinusPaper\Datenblattstefan.xls (-> MEAN "rul")

## Plot Increase
# Plot Acinar Increase
plot(Days,Increase,
  type="b",
#   ylim=c(0,1),
  main="Increase of Volumes compared to Day 4",
  xlab="Days",
  ylab="Increase",
  pch=2,
  col="red")
# Plot both increases (Stefan and ours)
plot(Days,StefansIncrease,
  ylim=c(0,max(max(Increase),max(StefansIncrease))),
  type="b",
  main="Increase of Volumes compared to Day 4",
  xlab="Days",
  ylab="Increase",
  pch=1,
  col="blue")
par(new=TRUE) # actually don't make a new plot    O.o
plot(Days,Increase,
  ylim=c(0,max(max(Increase),max(StefansIncrease))),
  type="b",
  axes=FALSE,
  main="",
  xlab="",
  ylab="",
  pch=2,
  col="red")
legend(list(x=50,y=5),legend = c("RLL","Acini"),pch=1:2,lty=1,col=c("blue","red"))

cat("Our increase is: ",sprintf("%.3f", Increase),"\n")
cat("Stefans increase is: ",sprintf("%.3f", StefansIncrease),"\n")

if ( PrintToTikZ == 1 ) {
    if (DivideThroughSize == 1) {
        tikz('_BoxPlotNormalized.tex', standAlone = TRUE)
        } else {
            tikz('_BoxPlot.tex', standAlone = TRUE)
        }
    }
boxplot(TotalVolumes,
  varwidth=TRUE,
  notch=TRUE,
  col="lightgray",
  names=Days,
  main="Boxplot of pooled Acinar Volumes of all measurements",
  xlab="Days after birth",
  )
if ( PrintToTikZ == 1 ) {  
    dev.off()
    cat("Saved Boxplot to _BoxPlot.tex in",getwd(),"\n")
}

cat("---\n")

# ## Put in for weeding out the POOLED Outliers
# for (i in 1:5) {
#  boxplot(TotalVolumes[,i],notch=TRUE,main=i)
# }
# sort(ConcatenatedVolumes[,,3],decreasing=TRUE)
# sort(TotalVolumes[,2],decreasing=TRUE)
summary(TotalVolumes) # give out Quantliles and other interesting stuff

if (DivideThroughSize == 1) {
        write.table(Mean,"_MeansNormalized.csv",sep=";")
        write.table(Median,"_MediansNormalized.csv",sep=";")
        write.table(TotalVolumes,"_TotalVolumesNormalized.csv",sep=";")
    } else {
        write.table(Mean,"_Means.csv",sep=";")
        write.table(Median,"_Medians.csv",sep=";")
        write.table(TotalVolumes,"_TotalVolumes.csv",sep=";")
    }

# summary(ConcatenatedVolumes[,,1])
# summary(ConcatenatedVolumes[,,2])
# summary(ConcatenatedVolumes[,,3])
# summary(ConcatenatedVolumes[,,4])
# summary(ConcatenatedVolumes[,,5])
# 
# t.test(ConcatenatedVolumes[,,1])
# t.test(ConcatenatedVolumes[,,2])
# t.test(ConcatenatedVolumes[,,3])
# t.test(ConcatenatedVolumes[,,4])
# t.test(ConcatenatedVolumes[,,5])
