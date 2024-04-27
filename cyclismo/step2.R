#2. Basic Data Types
#2.1. Variable Types
#2.1.1. Numbers
a <- 3
a #[1] 3
b <- sqrt(a*a+3)
b #[1] 3.464102
ls() #[1] "a" "b"

a <- c(1,2,3,4,5)
a #[1] 1 2 3 4 5
a+1 #[1] 2 3 4 5 6
mean(a) #[1] 3
var(a) #[1] 2.5

a <- c(1,2,3,4,5)
a[1] #[1] 1
a[2] #[1] 2
a[0] #numeric(0)
a[5] #[1] 5
a[6] #[1] NA

a <- numeric(10)
a #[1] 0 0 0 0 0 0 0 0 0 0

typeof(a) #[1] "double"



#2.1.2. Strings
a <- "hello"
a #[1] "hello"
b <- c("hello","there")
b #[1] "hello" "there"
b[1] #[1] "hello"

typeof(a) #[1] "character"
a = character(20)
a #[1] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""



#2.1.3. Factors
summary(tree$CHBR)

tree$C #[1]  1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 / [39] 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4
summary(tree$C)
tree$C <- factor(tree$C)
tree$C #[1]  1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 / [39] 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4
summary(tree$C)
levels(tree$C) #[1] "1" "2" "3" "4"



#2.1.4. Data Frames
a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
bubba <- data.frame(first=a,
                    second=b,
                    f=levels)
bubba
summary(bubba)
bubba$first #[1] 1 2 3 4
bubba$second #[1] 2 4 6 8
bubba$f #[1] A B A B / Levels: A B



#2.1.5. Logical
#<	less than / > great than / <= less than or equal / >= greater than or equal
# == equal to / != not equal to / | entry wise or / || or / ! not
# & entry wise and / && and / xor(a,b)	exclusive or

a = TRUE
typeof(a) #[1] "logical"
b = FALSE
typeof(b) #[1] "logical"

a = c(TRUE,FALSE)
b = c(FALSE,FALSE)
a|b #[1]  TRUE FALSE
a||b #[1] TRUE
xor(a,b) #[1]  TRUE FALSE

a = c(1,2,3)
is.numeric(a) #[1] TRUE
is.factor(a) #[1] FALSE



#2.2. Tables
#2.2.1. One Way Tables
a <- factor(c("A","A","B","A","B","B","C","A","C"))
results <- table(a)
results #a / A B C / 4 3 2
attributes(results)

summary(results) #Number of cases in table: 9 / Number of factors: 1

occur <- matrix(c(4,3,2),ncol=3,byrow=TRUE)
occur #[,1] [,2] [,3] / [1,]    4    3    2

colnames(occur) <- c("A","B","C")
occur #A B C / [1,] 4 3 2
occur <- as.table(occur)
occur #A B C / A 4 3 2
attributes(occur)



#2.2.2. Two Way Tables
a <- c("Sometimes","Sometimes","Never","Always","Always","Sometimes","Sometimes","Never")
b <- c("Maybe","Maybe","Yes","Maybe","Maybe","No","Yes","No")
results <- table(a,b)
results

sexsmoke<-matrix(c(70,120,65,140),ncol=2,byrow=TRUE)
rownames(sexsmoke)<-c("male","female")
colnames(sexsmoke)<-c("smoke","nosmoke")
sexsmoke <- as.table(sexsmoke)
sexsmoke