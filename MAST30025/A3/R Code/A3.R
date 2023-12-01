
library(tidyverse)
library(ggplot2)
library(MASS)

#Setting up Matrix Y
Y <- matrix(c(22,23,24,22,26,16,18,19,28,27,29,29),12,1)
Y

#Setting up matrix X
X <- matrix(rep(0,36),12,4)
X[,1] = 1 ;X[1:5,2]=1; X[6:8,3]=1; X[9:12,4]=1
X
library(Matrix)

#Computing X^tX
XtX <- t(X)%*%(X)
XtXc <- matrix(rep(0,36),4,4)
#Computing one of the conditional inverse
XtXc[2:4,2:4] <-solve(XtX[2:4,2:4])
XtXc



#Computing s2
df <- 12-3 #n=12 and reading X we can see r(X)=3 
I <- diag(12)
SSres <- t(Y)%*%(I-X%*%XtXc%*%t(X))%*%Y
SSres
s2 <- SSres/df




#Testing for estimablility using Thm 6.10
t1 <- matrix(c(1,2,1,0),4,1)
t(t1)%*%XtXc%*%XtX





#Computing Beta
b <- XtXc%*%t(X)%*%Y
b
#Computing 90% CI interval for 2nd type of bulb
t2 <- matrix(c(0,0,1,0),4,1)
tstat_halfalpha <- qt(0.95,df)
t2.std <- tstat_halfalpha*sqrt(s2*(t(t2)%*%XtXc%*%t2))
CI.90 <- c(t(t2)%*%b - t2.std,t(t2)%*%b + t2.std)
CI.90



#Testing for H_0:tau.1=tau.3 and H_1:tau.1!=tau.3 
C <- matrix(c(0,1,0,-1),1,4)
#C has a rank of 2
dst <- 0
numerator <- (t(C%*%b-dst)%*%solve(C%*%XtXc%*%t(C))%*%C%*%b-dst)
Fstat <- (numerator/2)/s2
pf(Fstat,2,df,lower=F)






#reading data_frame
library(ggplot2)
mile2 <- read.csv(file="mile2.csv")
mile2$Gender.f <- factor(mile2$Gender)

g1 <- ggplot(mile2, aes(x=Year, y=Time,colour=Gender.f)) + geom_point()
g2 <- g1 + labs(x="Gender", y="Time in second",
         title="World Record Time for the one mile Run 
from 1861–1999 for Male and 1967–1996 
for Female")
g2 + theme_bw()



#Determine if there are interaction using R
with(mile2, interaction.plot(Year,Gender.f, Time))
model <- lm(Time~Gender.f+Year, mile2)
imodel <- lm(Time~Gender.f+Year+Gender.f:Year, mile2)
anova(model,imodel)


#Plotting
g3 <- g2 + geom_smooth(method = lm)
g3


plot(mile2$Year, mile2$Time, pch=array(mile2$Gender.f), col=mile2$Gender.f)
abline(2309.4247,-1.0337, col="Red")
abline(953.7469,-0.3662, col="Blue")


#prediction
(1355.6777866)/(0.6675093)
C <- matrix(c(1,0,(1355.6777866)/(0.6675093),0),1,4)
C%*%imodel$coeff 
C <- matrix(c(1,1,(1355.6777866)/(0.6675093),(1355.6777866)/(0.6675093)),1,4)
C%*%imodel$coeff





#Direct Calculation
Y <- mile2$Time
n <- length(Y)
X <- matrix(0,n,6)
X[,1] <- 1
X[cbind(1:n,as.numeric(mile2$Gender.f)+1)] <-1
X[,4] <- mile2$Year
X[cbind(1:n,as.numeric(mile2$Gender.f)+4)] <- mile2$Year
XtX <- t(X)%*%X
r <- rankMatrix(X)[1]
XtXc <- matrix(0,6,6)
M <- XtX[c(2:3,5:6),c(2:3,5:6)]
XtXc[c(2:3,5:6),c(2:3,5:6)] <-  solve(M)
b <- XtXc%*%t(X)%*%Y
b


(1355.6777866)/(0.6675093)
t1 <- matrix(c(1,1,0,1,(1355.6777866)/(0.6675093),0),6,1)
t(t1)%*%b
t2 <- matrix(c(1,0,1,1,0,(1355.6777866)/(0.6675093)),6,1)
t(t2)%*%b


#Computing 95% CI
df=n-r
s2 <- sum((Y - X%*%b)^2)/df
t3 <- matrix(c(0,0,0,0,1,-1),6,1)
tstat_halfalpha <- qt(0.975,df)
t3.std <- tstat_halfalpha*sqrt(s2*(t(t3)%*%XtXc%*%t3))
CI.95 <- c(t(t3)%*%b - t3.std,t(t3)%*%b + t3.std)
CI.95

library(gmodels)
estimable(imodel, c(0,0,0,1), conf.int=0.95)
confint(imodel)


#Manual Calculation
C <- matrix(c(0,0,0,0,0,1),1,6)
#C has a rank of 1
dst <- -0.4
numerator <- t(C%*%b-dst)%*%solve(C%*%XtXc%*%t(C))%*%(C%*%b-dst)
Fstat <- (numerator)/s2
Fstat
pf(Fstat,1,df,lower=F)






#Performing random selection
n <- c(12,12,16)
nsum <- sum(n)
x <- sample(nsum, nsum)
n1 <- x[1:n[1]]
n2 <- x[n[1]+1:n[2]]
n3 <- x[n[2]+1:n[3]]
n1
n2
n3
