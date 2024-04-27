#4. Basic Probability Distributions
#4.1. The Normal Distribution
dnorm(0) #[1] 0.3989423
dnorm(0)*sqrt(2*pi) #[1] 1
dnorm(0,mean=4) #[1] 0.0001338302
dnorm(0,mean=4,sd=10) #[1] 0.03682701
v <- c(0,1,2)
dnorm(v) #[1] 0.39894228 0.24197072 0.05399097
x <- seq(-20,20,by=.1)
y <- dnorm(x)
plot(x,y)
y <- dnorm(x,mean=2.5,sd=0.1)
plot(x,y)

pnorm(0) #[1] 0.5
pnorm(1) #[1] 0.8413447
pnorm(0,mean=2) #[1] 0.02275013
pnorm(0,mean=2,sd=3) #[1] 0.2524925
v <- c(0,1,2)
pnorm(v) #[1] 0.5000000 0.8413447 0.9772499
x <- seq(-20,20,by=.1)
y <- pnorm(x)
plot(x,y)
y <- pnorm(x,mean=3,sd=4)
plot(x,y)

pnorm(0,lower.tail=FALSE) #[1] 0.5
pnorm(1,lower.tail=FALSE) #[1] 0.1586553
pnorm(0,mean=2,lower.tail=FALSE) #[1] 0.9772499

qnorm(0.5) #[1] 0
qnorm(0.5,mean=1) #[1] 1
qnorm(0.5,mean=1,sd=2) #[1] 1
qnorm(0.5,mean=2,sd=2) #[1] 2
qnorm(0.5,mean=2,sd=4) #[1] 2
qnorm(0.25,mean=2,sd=2) #[1] 0.6510205
qnorm(0.333) #[1] -0.4316442
qnorm(0.333,sd=3) #[1] -1.294933
qnorm(0.75,mean=5,sd=2) #[1] 6.34898
v = c(0.1,0.3,0.75)
qnorm(v) #[1] -1.2815516 -0.5244005  0.6744898
x <- seq(0,1,by=.05)
y <- qnorm(x)
plot(x,y)
y <- qnorm(x,mean=3,sd=2)
plot(x,y)
y <- qnorm(x,mean=3,sd=0.1)
plot(x,y)

rnorm(4) #[1]  1.2387271 -0.2323259 -1.2003081 -1.6718483
rnorm(4,mean=3) #[1] 2.633080 3.617486 2.038861 2.601933
rnorm(4,mean=3,sd=3) #[1] 4.580556 2.974903 4.756097 6.395894
rnorm(4,mean=3,sd=3) #[1]  3.000852  3.714180 10.032021  3.295667
y <- rnorm(200)
hist(y)
y <- rnorm(200,mean=-2)
hist(y)
y <- rnorm(200,mean=-2,sd=4)
hist(y)
qqnorm(y)
qqline(y)



#4.2. The t Distribution
help(TDist)

x <- seq(-20,20,by=.5)
y <- dt(x,df=10)
plot(x,y)
y <- dt(x,df=50)
plot(x,y)

pt(-3,df=10) #[1] 0.006671828
pt(3,df=10) #[1] 0.9933282
1-pt(3,df=10) #[1] 0.006671828
pt(3,df=20) #[1] 0.996462
x = c(-3,-4,-2,-1)
pt((mean(x)-2)/sd(x),df=20) #[1] 0.001165548
pt((mean(x)-2)/sd(x),df=40) #[1] 0.000603064

qt(0.05,df=10) #[1] -1.812461
qt(0.95,df=10) #[1] 1.812461
qt(0.05,df=20) #[1] -1.724718
qt(0.95,df=20) #[1] 1.724718
v <- c(0.005,.025,.05)
qt(v,df=253) #[1] -2.595401 -1.969385 -1.650899
qt(v,df=25) #[1] -2.787436 -2.059539 -1.708141

rt(3,df=10) #[1] 0.9440930 2.1734365 0.6785262
rt(3,df=20) #[1]  0.1043300 -1.4682198  0.0715013
rt(3,df=20) #[1]  0.8023832 -0.4759780 -1.0546125



#4.3. The Binomial Distribution
help(Binomial)

x <- seq(0,50,by=1)
y <- dbinom(x,50,0.2)
plot(x,y)
y <- dbinom(x,50,0.6)
plot(x,y)
x <- seq(0,100,by=1)
y <- dbinom(x,100,0.6)
plot(x,y)

pbinom(24,50,0.5) #[1] 0.4438624
pbinom(25,50,0.5) #[1] 0.5561376
pbinom(25,51,0.5) #[1] 0.5
pbinom(26,51,0.5) #[1] 0.610116
pbinom(25,50,0.5) #[1] 0.5561376
pbinom(25,50,0.25) #[1] 0.999962
pbinom(25,500,0.25) #[1] 4.955658e-33

qbinom(0.5,51,1/2) #[1] 25
qbinom(0.25,51,1/2) #[1] 23
pbinom(23,51,1/2) #[1] 0.2879247
pbinom(22,51,1/2) #[1] 0.200531

rbinom(5,100,.2) #[1] 30 23 21 19 18
rbinom(5,100,.7) #[1] 66 66 58 68 63



#4.4. The Chi-Squared Distribution
help(Chisquare)

x <- seq(-20,20,by=.5)
y <- dchisq(x,df=10)
plot(x,y)
y <- dchisq(x,df=12)
plot(x,y)

pchisq(2,df=10) #[1] 0.003659847
pchisq(3,df=10) #[1] 0.01857594
1-pchisq(3,df=10) #[1] 0.981424
pchisq(3,df=20) #[1] 4.097501e-06
x = c(2,4,5,6)
pchisq(x,df=20) #[1] 1.114255e-07 4.649808e-05 2.773521e-04 1.102488e-03

qchisq(0.05,df=10) #[1] 3.940299
qchisq(0.95,df=10) #[1] 18.30704
qchisq(0.05,df=20) #[1] 10.85081
qchisq(0.95,df=20) #[1] 31.41043
v <- c(0.005,.025,.05)
qchisq(v,df=253) #[1] 198.8161 210.8355 217.1713
qchisq(v,df=25) #[1] 10.51965 13.11972 14.61141

rchisq(3,df=10) #[1] 16.80075 20.28412 12.39099
rchisq(3,df=20) #[1] 17.838878  8.591936 17.486372
rchisq(3,df=20) #[1] 11.19279 23.86907 24.81251