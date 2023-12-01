
#Question 2

#Pre Question
#Loading Data
setwd("~/Desktop/University Works & Notes/2021/Semester 1/Linear Statistical Models/Assignment/A2/R Code")
q2data <- data.matrix(read.csv(file = "Q2_Data.csv"))
q2frame <- read.csv(file="Q2_Data.csv")

#Viewing data frame
pairs(q2frame)

#Defininting X and Y
y <- matrix(q2data[,1],7,1)
y
x <- matrix(c(rep(1,7),q2data[,-1]),7,4)
x
df <- 7-4

#Finding Beta
b <- solve(t(x)%*%x,t(x)%*%y)
b

#Finding variance
#sum-Square
e <- (y-x%*%b)
SSres <- sum(e^2)
s2 <- SSres/(df)
s <- sqrt(s2)

#Beta Variance
C2x <- solve(t(x)%*%x)*s2
diag(C2x)
sqrt(diag(C2x))



#Part B Computing CI

alpha <- 0.1
x.star <- c(1,10,100,6)
y.star <- x.star%*%b
ta <- qt(1-alpha/2, df)

#90CI for x1=10, x2= 100 ,x3 =6

CI = c(y.star - s*sqrt(t(x.star)%*%solve(t(x)%*%x)%*%x.star),y.star + s*sqrt(t(x.star)%*%solve(t(x)%*%x)%*%x.star))
CI


#Part C

#General Linear Hypothesis for B1-B3
C <- c(0,1,0,-1)

#Computing the variance and standard error for B1-B3
Cb.var <- t(C)%*%solve(t(x)%*%x)%*%C*s2
Cb.var
Cb.ste <- sqrt(Cb.var)
Cb.ste




#Part D
#General Linear Hypothesis
C <- matrix(c(0,1,0,0),1,4)
dst <- matrix(-1)

#Computing the variance and standard error for y=-1 given B1=-1
num <- (t(C%*%b-dst)*solve((C%*%solve(t(x)%*%x)%*%t(C)))%*%(C%*%b-dst))
Fstat <- num/(SSres/df)
pf(Fstat,1,3, lower.tail = FALSE)




#Part E
#Computing new model
x2 <- x[,-1]
b2 <- solve(t(x2)%*%x2,t(x2)%*%y)

#Breaking Rg1g2 and Rg2
SSres2 <- sum((y-x2%*%b2)^2)
Rg2 <- t(y)%*%x2%*%b2
SSreg <- t(y)%*%y
Rg1g2 <- SSreg - Rg2
Rg1g2

#F Test
r <- 1
Fstat <- (Rg1g2/r)/(SSres/(df))
Fstat
pf(Fstat,r,df, lower.tail=FALSE)




#Double Checking
model1 <- lm(price.y. ~ x1+x2+x3, data = q2frame)
summary(model1)
linearHypothesis(model1,c,dst)
anova(model1)
library(car)



#Question 4
data(mtcars)
mtcars
mtcars.new = log(mtcars[, c(1,3:7)])
mtcars.new

#Part A: Plotting Data
pairs(mtcars.new)


#Part B: Forward Selection

basemodel <- lm(mpg~1, data=mtcars.new)
add1(basemodel, scope = ~.+disp+hp+drat+wt+qsec, test="F")
q4model2 <- lm(mpg ~ disp, data=mtcars.new)
add1(q4model2, scope = ~.+hp+drat+wt+qsec, test="F") 
q4model3 <- lm(mpg~disp+wt, data=mtcars.new)
add1(q4model3, scope = ~.+hp+drat+qsec, test="F") 
q4model4 <- lm(mpg~disp+hp+wt, data=mtcars.new)
add1(q4model4, scope = ~.+drat+qsec, test="F") 
summary(q4model4)


#Part C:
AICbasemodel <- lm(mpg ~ disp+hp+drat+wt+qsec ,data=mtcars.new)
q4modelAIC <- step(AICbasemodel, scope = ~., steps=4)



#Part D:
summary(q4modelAIC)

#Part E:
plot(q4modelAIC)




#Question 5:

#Part C:

f1 <-function(lambda){
  #Calling Data Frame
  q2data <- data.matrix(read.csv(file = "Q2_Data.csv"))
  q2frame <- read.csv(file="Q2_Data.csv")
  #Converting Data frame to matrix
  y <- matrix(q2data[,1],7,1)
  x <- matrix(c(rep(1,7),q2data[,-1]),7,4)
  #Scaling data
  x <- scale(x[,-1],center=T,scale=T)
  y <- scale(y,center=T,scale=T)
  p <- 3
  #Definting Degree of freedom
  lambda = matrix(c(lambda,0,0,0,lambda,0,0,0,lambda),3,3)
  H <- x%*%solve(t(x)%*%x+lambda)%*%t(x)
  df <- sum(diag(H))
  #Computing Beta
  b <- solve((t(x)%*%x)+lambda)%*%t(x)%*%y
  #Computing Sum-squared
  e <- (y-x%*%b)
  SSres <- sum(e^2)
  n <- dim(y)[1]
  gof <- n*log(SSres/n)+2*df
  return(gof)
}


