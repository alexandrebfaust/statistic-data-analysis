#3. Basic Operations and Numerical Descriptions
#3.1. Basic Operations
a <- c(1,2,3,4)
a #[1] 1 2 3 4
a + 5 #[1] 6 7 8 9
a - 10 #[1] -9 -8 -7 -6
a*4 #[1]  4  8 12 16
a/5 #[1] 0.2 0.4 0.6 0.8

b <- a - 10
b #[1] -9 -8 -7 -6

sqrt(a) #[1] 1.000000 1.414214 1.732051 2.000000
exp(a) #[1]  2.718282  7.389056 20.085537 54.598150
log(a) #[1] 0.0000000 0.6931472 1.0986123 1.3862944
exp(log(a)) #[1] 1 2 3 4

c <- (a + sqrt(a))/(exp(2)+1)
c #[1] 0.2384058 0.4069842 0.5640743 0.7152175

a + b #[1] -8 -6 -4 -2

a*b #[1]  -9 -16 -21 -24
a/b #[1] -0.1111111 -0.2500000 -0.4285714 -0.6666667
(a+3)/(sqrt(1-b)*2-1) #[1] 0.7512364 1.0000000 1.2884234 1.6311303

a <- c(1,2,3)
b <- c(10,11,12,13)
a+b #[1] 11 13 15 14
#Warning message:  longer object length is not a multiple of shorter object length in: a + b

ls()
#[1] "a"            "b"            "bubba"        "c"            "last.warning"
#[6] "tree"         "trees"

a <- c(1,-2,3,-4)
b <- c(-1,2,-3,4)
min(a,b) #[1] -4
pmin(a,b) #[1] -1 -2 -3 -4



#3.2. Basic Numerical Descriptions
tree <- read.csv(file="../src/trees91.csv",header=TRUE,sep=",");
names(tree)
tree$LFBM

mean(tree$LFBM) #[1] 0.7649074
median(tree$LFBM) #[1] 0.72
quantile(tree$LFBM) #0%    25%    50%    75%   100% / 0.1300 0.4800 0.7200 1.0075 1.7600
min(tree$LFBM) #[1] 0.13
max(tree$LFBM) #[1] 1.76
var(tree$LFBM) #[1] 0.1429382
sd(tree$LFBM) #[1] 0.3780717

summary(tree$LFBM)

summary(tree)



#3.3. Operations on Vectors
a = c(2,4,6,3,1,5)
b = sort(a)
c = sort(a,decreasing = TRUE)
a #[1] 2 4 6 3 1 5
b #[1] 1 2 3 4 5 6
c #[1] 6 5 4 3 2 1

min(a) #[1] 1
max(a) #[1] 6

sum(a) #[1] 21