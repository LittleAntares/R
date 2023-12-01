setwd("C:/Users/Kim Seang CHY/Desktop/School Work/USB")


#Question 1
#Entering the sample data with y being the number of respondents, and n being the sample size
y <- c(520, 600) 
n <- c(800, 1000)


#Two-sided proportion test and produce a confidence level of 0.95
prop.test(y, n, alternative = "two.sided", correct = FALSE) 



#Question 2:
#Conducting a hypothesis testing for comparing variance and it 95% CI
x <- c(12.1, 12.2, 17.4, 13.1, 17.8, 19.8, 13, 10.8, 18.4, 16)
y <- c(20.1, 21.3, 20.4, 21.7, 20.3 ,19.5, 19.4, 19.9)
var.test(x, y, alternative = "two.sided")


#Question 3
coffee <- read.csv("coffee.csv") #Opening up data
customer <- coffee$customer # Redefining varible
sale <- coffee$sales #Redefining varible

#Fit the model
model1 <- lm(sales ~ customer, data = coffee) 

# Show result
summary(model1) 

#finding 95% confidence interval for regression coefficient
confint(model1)

customer_number <- data.frame(customer=100) # Data to use for prediction

#Calculating 95% confidence interval for 
predict(model1, newdata = customer_number, interval = "confidence")

#Calculating 95% prediction interval for 
predict(model1, newdata = customer_number, interval = "prediction")


#plotting data vs fitted model
plot(customer, sale , col = 'blue', pch =16, 
     main = "Sale against Number of Customer", xlab = "Number of Customer",
     ylab = "Sales")
abline(model1, col= "red", lwd='3')

#Plotting residual vs Fitted and plotting QQ plot of the residual
par(mfrow = c(1,2))
plot(model1, 1:2)


