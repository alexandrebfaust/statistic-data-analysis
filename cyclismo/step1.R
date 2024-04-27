setwd("I:/mestrado/statistic-data-analysis")

bubba <- c(3,5,7,9)
bubba #[1] 3 5 7 9
bubba[0] #numeric(0)
bubba[1] #[1] 3
bubba[2] #[1] 5
bubba[3] #[1] 7
bubba[4] #[1] 9
bubba[5] #[1] NA

heisenberg <- read.csv(file="../src/simple.csv",head=TRUE,sep=",")
heisenberg
summary(heisenberg)

help(read.csv)

dir() #[1] "simple.csv" "step1.R"
getwd() #[1] "I:/mestrado/statistic-data-analysis"

heisenberg$trial #[1] A A B B A B #Levels: A B
heisenberg$mass #[1] 10.0 11.0  5.0  6.0 10.5  7.0
heisenberg$velocity #[1] 12 14  8 10 13 11

names(heisenberg) #[1] "trial" "mass" "velocity"

tree <- read.csv(file="../src/trees91.csv",header=TRUE,sep=",");
attributes(tree)
names(tree)

tree$C


help(read.fwf)

a = read.fwf('../src/fixedWidth.dat',widths=c(-17,15,7),col.names=c('temp','offices'))
a