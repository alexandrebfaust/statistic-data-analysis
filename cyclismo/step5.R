setwd("I:/mestrado/statistic-data-analysis")
#5. Basic Plots
w1 <- read.csv(file="../src/w1.dat",sep=",",head=TRUE)
names(w1) #[1] "vals"
tree <- read.csv(file="../src/trees91.csv",sep=",",head=TRUE)
names(tree)
#[1] "C"      "N"      "CHBR"   "REP"    "LFBM"   "STBM"   "RTBM"   "LFNCC"
#[9] "STNCC"  "RTNCC"  "LFBCC"  "STBCC"  "RTBCC"  "LFCACC" "STCACC" "RTCACC"
#[17] "LFKCC"  "STKCC"  "RTKCC"  "LFMGCC" "STMGCC" "RTMGCC" "LFPCC"  "STPCC"
#[25] "RTPCC"  "LFSCC"  "STSCC"  "RTSCC"



#5.1. Strip Charts
help(stripchart)
stripchart(w1$vals)
stripchart(w1$vals,method="stack")
stripchart(w1$vals,method="jitter")
stripchart(w1$vals,method="stack",
           main='Leaf BioMass in High CO2 Environment',
           xlab='BioMass of Leaves')
title('Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves')



#5.2. Histograms
hist(w1$vals)
hist(w1$vals,main="Distribution of w1",xlab="w1")

help(hist)

hist(w1$vals,breaks=2)
hist(w1$vals,breaks=4)
hist(w1$vals,breaks=6)
hist(w1$vals,breaks=8)
hist(w1$vals,breaks=12)

hist(w1$vals,breaks=12,xlim=c(0,10))
hist(w1$vals,breaks=12,xlim=c(-1,2))
hist(w1$vals,breaks=12,xlim=c(0,2))
hist(w1$vals,breaks=12,xlim=c(1,1.3))
hist(w1$vals,breaks=12,xlim=c(0.9,1.3))

hist(w1$vals,
     main='Leaf BioMass in High CO2 Environment',
     xlab='BioMass of Leaves')
title('Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves')

hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
stripchart(w1$vals,add=TRUE,at=15.5)



#5.3. Boxplots
boxplot(w1$vals)
boxplot(w1$vals,
        main='Leaf BioMass in High CO2 Environment',
        ylab='BioMass of Leaves')

help(boxplot)

boxplot(w1$vals,
        main='Leaf BioMass in High CO2 Environment',
        xlab='BioMass of Leaves',
        horizontal=TRUE)

hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
boxplot(w1$vals,horizontal=TRUE,at=15.5,add=TRUE,axes=FALSE)

hist(w1$vals,main='Leaf BioMass in High CO2 Environment',xlab='BioMass of Leaves',ylim=c(0,16))
boxplot(w1$vals,horizontal=TRUE,at=16,add=TRUE,axes=FALSE)
stripchart(w1$vals,add=TRUE,at=15)

tree <- read.csv(file="../src/trees91.csv",sep=",",head=TRUE)
tree$C <- factor(tree$C)
tree$N <- factor(tree$N)

boxplot(tree$STBM,
        main='Stem BioMass in Different CO2 Environments',
        ylab='BioMass of Stems')

boxplot(tree$STBM~tree$C)



#5.4. Scatter Plots
plot(tree$STBM,tree$LFBM)
cor(tree$STBM,tree$LFBM) #[1] 0.911595
plot(tree$STBM,tree$LFBM,
     main="Relationship Between Stem and Leaf Biomass",
     xlab="Stem Biomass",
     ylab="Leaf Biomass")



#5.5. Normal QQ Plots
qqnorm(w1$vals)
qqnorm(w1$vals,
       main="Normal Q-Q Plot of the Leaf Biomass",
       xlab="Theoretical Quantiles of the Leaf Biomass",
       ylab="Sample Quantiles of the Leaf Biomass")

qqline(w1$vals)