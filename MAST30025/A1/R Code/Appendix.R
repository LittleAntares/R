#Question 5

#Part A
#Setting Data Point
y <- matrix(c(27.3, 42.4 ,38.7, 4.5, 23, 166.3, 109.7, 80.1, 150.7, 20.3, 189.7,
              131.3, 404.2, 149),14,1,byrow = TRUE)
y

x <- matrix(c(rep(1,14),13.1, 15.3, 25.8, 1.8, 4.9, 55.4, 
              39.3, 26.7, 47.5, 6.6, 94.7, 61.1, 135.6, 47.6), 14, 2)
x

#Part B
#Finding Least Square Estimator
b <- solve(t(x)%*%x,t(x)%*%y)
b

#Inverse of diagonal X^T*X
xtx.inverse <- solve(t(x)%*%x)
xtx.inverse

#Part C
#Calculating Sample Variance
e <- y-x%*%b
e

#Calculating Sum-squared
SSRes <- sum(e^2)
SSRes

#Calculating sample variance
s2 <- SSRes/(14-2)
s2

#Calculating the variance of the least square estimator
b.var <- solve(t(x)%*%x)*s2
diag.b.var <- diag(solve(t(x)%*%x))*s2
diag.b.var

#Part D
#Calculating ocean trout expected price in 1980
t <- matrix(c(1,28),2,1)
t
y.bar <- t(t)%*%b
y.bar

#Part E
#Calculating the H(hat) matrix
hat <- x%*%solve(t(x)%*%x)%*%t(x)
hat

#Sea scallops leverage
hat[13,13]

#Calculating Residual variance
e.var <- s2*(diag(14)-hat)
e.var

#Standardised Residual calculation function
z <- function(i){
  e[i,1]/sqrt(s2*(1-hat[i,i]))
}

#Calculating Standardised Residual for sea scallops
z(13)

#Part F
#Cook's distance functioon 
d <-function(i){
  (((z(i))^2)/2)*((hat[i,i])/(1-hat[i,i]))
} 

#Cook's distance for sea scallops
d(13)



#Part G
#Cook's Distance matrix for all observation
cook.d <- matrix(c(0,14), 14, 1)
i <- 0
while (i < 15) {
  cook.d[i,1]=d(i)
  i = i+1
}
cook.d



#Omitting Sea Scallop observation fitting
x.omit <- x[-13,]
x.omit
y.omit <- y[-13,1]
y.omit
b.omit <- solve(t(x.omit)%*%x.omit,t(x.omit)%*%y.omit)
b.omit


#Plotting Regression Graph with and without sea scallop
plot(x[,2],y)
abline(b[1,1],b[2,1], col = "blue")
abline(b.omit[1,1],b.omit[2,1], col = "red")
legend("topleft",
       c("With Sea Scallops","Without Sea Scallop"),
       fill=c("blue","red"))






