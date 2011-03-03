##################################################
# example from http://www.cyclismo.org/tutorial/R/plotting.html
##################################################

setwd("P:/doc/#R/TreeTest")

# Setup Data
w1 <- read.csv(file="w1.dat",sep=",",head=TRUE)
names(w1)
tree <- read.csv(file="trees91.csv",sep=",",head=TRUE)
names(tree)

# Simplest Chart
stripchart(w1$vals)
stripchart(w1$vals,method="stack")
stripchart(w1$vals,method="jitter")
stripchart(w1$vals,vertical=TRUE)
stripchart(w1$vals,vertical=TRUE,method="jitter")
stripchart(w1$vals,method="stack",
  main='Leaf BioMass in High CO2 Environment',
  xlab='BioMass of Leaves')
  
## Histogram  
hist(w1$vals)
hist(w1$vals,breaks=5)
hist(w1$vals,breaks=12,xlim=c(0.9,1.3))
hist(w1$vals,
main='Leaf BioMass in High CO2 Environment',
  xlab='BioMass of Leaves')
  
## Add two plots on top of each other
hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
stripchart(w1$vals,add=TRUE,at=15.5)

## Boxplotting
boxplot(w1)
boxplot(w1$vals,
  main='Leaf BioMass in High CO2 Environment',
  ylab='BioMass of Leaves')
boxplot(w1$vals,
  main='Leaf BioMass in High CO2 Environment',
  xlab='BioMass of Leaves',
  horizontal=TRUE)
# Histogram combined with Boxplot
hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
boxplot(w1$vals,horizontal=TRUE,at=15.5,add=TRUE,axes=FALSE)
# All three plots combined  
hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
boxplot(w1$vals,horizontal=TRUE,at=16,add=TRUE,axes=FALSE)
stripchart(w1$vals,add=TRUE,at=15)  
# Boxplotting the other Dataset
tree <- read.csv(file="trees91.csv",sep=",",head=TRUE)
tree$C <- factor(tree$C)
tree$N <- factor(tree$N)
# Boxplot for all Trees
boxplot(tree$STBM,
  main='Stem BioMass in Different CO2 Environments',
  ylab='BioMass of Stems')
# Boxplot for different environments
boxplot(tree$STBM~tree$C)

## Scatter Plots
plot(tree$STBM,tree$LFBM)
# Is there a correlation?
cor(tree$STBM,tree$LFBM)
# Annotate Plot
plot(tree$STBM,tree$LFBM,
  main="Relationship Between Stem and Leaf Biomass",
  xlab="Stem Biomass",
  ylab="Leaf Biomass")
  
# normal quantile Plot  
qqnorm(w1$vals)
qqnorm(w1$vals,
  main="Normal Q-Q Plot of the Leaf Biomass",
  xlab="Theoretical Quantiles of the Leaf Biomass",
  ylab="Sample Quantiles of the Leaf Biomass")
# Add the theoretical line
qqline(w1$vals)









