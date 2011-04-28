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

## Setup stuff
DoPlots=0       # 1 does the Plots, something else doen't
DoBoxplots=1    # 1 does the BoxPlots, something else doen't
DoBoxplotsWithoutOutliers=0    # 1 does the BoxPlots without the Outliers, something else doen't
Days <- c(04,10,21,36,60)

Days <- 04
# Days <- 10
# Days <- 21
# Days <- 36
# Days <- 60

## Initialize empty Variables 
Time <- format(Sys.time(), "%d.%b %H:%M")
Mean <- NaN
GlobalMean <- NaN
NumberOfCounts <- NaN
NumberOfAcini <- NaN

## Loop through data in Days construct FileName for XLS-File
for (currentDay in 1:length(Days)) { # Iterate trough the days
    ## Construct Day, FileName and read/print relevant Info to console
    Day <- Days[currentDay]
    cat("---\n")
    cat("Working on Data from Day", Day, "\n")
    Day <- sprintf("%02.0f", Day) # Format the Day nicely, so we can read the Filenames correctly
    FileName <- paste("R108C",Day,".xls",sep="")
    FileLocation <- paste("p:/doc/#Tables/AcinarTreeExtraction/",FileName,sep="")
    SheetNames <- sheetNames(FileLocation) # Give out Information about read XLS-File
    cat(FileName, "contains", sheetCount(FileLocation), "Sheets with the following names:\n")
    for (i in 1:length(SheetNames)) print(SheetNames[i])
  
    cat("---\n")
    ## In a first loop, read out all the sheets in the file and plot the volume data
    cat("Extracting single Volumes\n")        
    for (i in 1:length(SheetNames)) { # Iterate through every sheet in the current XLS-File
        # cat("reading Sheet #",i," named '",SheetNames[i],"'\n",sep="")
        Data <- read.xls(FileLocation,sheet=i) # read Sheet Nr. i
###################### REMOVE OUTLIERS
# NEEDS SOME MORE LOOKING INTO!
#         if (!is.na(Data$Volume[1])) {
#             Data$Volume<-rm.outlier(Data$Volume,fill=TRUE)
#             cat("REMOVING OUTLIERS FOR", SheetNames[i], "\n")
#         }
###################### REMOVE OUTLIERS
        Mean[i] <- mean(Data$Volume,na.rm=TRUE) # Calculate the arithmetic mean without the empty cells and save into current Mean
        NumberOfCounts[i] <- length(Data$Volume)
        NumberOfAcini[i] <- length(na.exclude(Data$Volume))
        if (!is.nan(Mean[i])) { # Plot the Data for Acini where we actually have Data (i.e., where Mean[i] is not empty)
            if (DoPlots==1) {
                plot(Data$Volume,type="b",col="red")
                PlotTitle <- paste(SheetNames[i],"|", NumberOfAcini[i], "Acini | Mean:", sprintf("%.4f", Mean[i]), "\n")
                abline(h=Mean[i], col = "gray60") # Plot Mean[i]
                title(PlotTitle)
            }
            cat("For", SheetNames[i], "we counted",
                NumberOfAcini[i], "Acini with a mean volume of",
                Mean[i], "(and omitted",sum(is.na(Data$Volume)),"empty counts).\n") # Give out something to read in the console
            } # end if Mean[i] is not empty
            if (is.nan(Mean[i])) { # if Mean[i] is empty, we probably have an empty Sheet...
                cat("For", SheetNames[i], "we counted no Acini, which means that the sheet is probably emtpy.\n")
            } # end if Mean[i] *is* empty
        } # end iterate through sheets

    cat("---\n")    
    ## In a second loop below, read out all the data again (could probably be done in the same loop), concatenate into an Array and use this for a Boxplot
    if (DoBoxplots==1) {
        cat("Extracting single Volumes and saving them into an array for boxplotting\n")
        ConcatenatedVolumes = array(NaN,c(max(NumberOfCounts),length(SheetNames))) # Initialize Array for Volumes
        GlobalMean[currentDay] = mean(Mean,na.rm=TRUE)        
        for (i in 1:length(SheetNames)) { # Iterate through every sheet in the current XLS-File
            cat("Working on sheet ",i,"/",length(SheetNames),"\n",sep="")
            Data <- read.xls(FileLocation,sheet=i) # read Sheet Nr. i
            ConcatenatedVolumes[1:length(Data$Volume),i] = Data$Volume # put just read Data into ith column of Array
        } # end iterate through sheets
        BoxPlotTitle <- paste(FileName,"| total Acini:", sum(NumberOfAcini), "| global Mean:", sprintf("%.4f", mean(Mean,na.rm=TRUE)), "|", Time, "\n")
        boxplot(ConcatenatedVolumes,
            notch=TRUE,
            varwidth=TRUE,
            col="lightgray",
            outline=TRUE, # Plot plot outliers
            names=SheetNames,
            main=BoxPlotTitle)
        abline(h=GlobalMean[currentDay], col = "red")
        if (DoBoxplotsWithoutOutliers==1) {
            BoxPlotTitle <- paste(FileName,"| total Acini:", sum(NumberOfAcini), "| global Mean:", sprintf("%.4f", mean(Mean,na.rm=TRUE)), "|", Time, "| NO OUTLIERS (->MEAN)!\n")
            boxplot(ConcatenatedVolumes,
                notch=TRUE,
                varwidth=TRUE,
                col="lightgray",
                outline=FALSE, # Don't plot outliers
                names=SheetNames,
                main=BoxPlotTitle)
            abline(h=GlobalMean[currentDay], col = "red")
        }
        cat("---\n")
    }
} # end iterate through Days

GlobalMean

Increase <- GlobalMean/GlobalMean[1]