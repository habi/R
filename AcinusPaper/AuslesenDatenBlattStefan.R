rm(list=ls()) # clear EVERYTHING!
setwd("p:/doc/#R/AcinusPaper")
library(gdata) # needed to read XLS-Files (needs an installation of PERL: http://www.perl.org/get.html)


StefansVolumes = array(NaN,c(5,5),dimnames = list(c(1,2,3,4,5), c("4", "10", "21", "36", "60")))
cat("Reading RUL-Volumes in 'Datenblattstefan.xls' Day 04\n")
TMP <- read.xls("Datenblattstefan.xls", sheet="DAY 4")
StefansVolumes[,1]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]]) # was read in as Factor, according to http://is.gd/QHQpCH, this makes values numeric.
cat("Reading RUL-Volumes in 'Datenblattstefan.xls' Day 10\n")
TMP <- read.xls("Datenblattstefan.xls", sheet="DAY 10")
StefansVolumes[,2]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]])
cat("Reading RUL-Volumes in 'Datenblattstefan.xls' Day 21\n")
TMP <- read.xls("Datenblattstefan.xls", sheet="DAY 21")
StefansVolumes[,3]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]])
cat("Reading RUL-Volumes in 'Datenblattstefan.xls' Day 36\n")
TMP <- read.xls("Datenblattstefan.xls", sheet="DAY 36")
StefansVolumes[,4]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]])
cat("Reading RUL-Volumes in 'Datenblattstefan.xls' Day 60\n")
TMP <- read.xls("Datenblattstefan.xls", sheet="DAY 60")
StefansVolumes[,5]<-as.numeric(levels(TMP$X.5[11:15])[TMP$X.5[11:15]])