
#############################################################################
#Question 1.a
#############################################################################
plant_height<- c(173.1, 61.5, 123.3, 100.4, 20.4, 20.9, 
        228.4, 1, 6.8, 11.4, 7.7, 40.7, 
        15.8,422.4, 58.2, 19.9, 38.8, 121, 
        118.6, 174.9, 87.2, 14, 204.7, 81.9,
        57.3, 177, 14.1, 137, 76.4, 330.2)
#Entering the plant height data into one rows vector named plant_height


summary(plant_height) #Providing a five number summary plus the mean value

boxplot(plant_height, horizontal=TRUE, xlab="Plant Height in cm", ylab="Plant") 
#Creating a simple horizontal box plots from the plant height data


#############################################################################
#Question 1.b
#############################################################################
library(MASS) #Loading Mass libra

exponential_fit <- fitdistr(x = plant_height, densfun = "exponential")
exponential_fit #Computing maximum likelihood estimator of the exponential distribution



#############################################################################
#Question 1.c
#############################################################################
set.seed(1)
gen_exp <- rexp(n=100, rate=1/100) #Simulating 1000 of x~Exp(100)
x <- quantile(gen_exp,probs = seq(0,1, 0.05), type = 6)#Converting simulation  to type 6 Quantile
y <- quantile(plant_height,probs = seq(0,1, 0.05), type = 6) #Converting plant data to type 6 Quantile
plot(x, y, xlab = "X~Exp(100) Quantiles", ylab = "Sample Quantiles") #Plotting QQ-Graph
fitline <- lm(y~x) #Computing best fit line
abline(fitline, col="Blue", lwd=2) #Drawing best fit line


#############################################################################
#Question 1.d
#############################################################################
gen_exp <- rexp(n=100, rate=1) #Simulating 1000 of x~Exp(100)
x <- quantile(gen_exp,probs = seq(0,1, 0.05), type = 6)#Converting simulation  to type 6 Quantile
fitline <- lm(y~x) #Computing best fit line
summary(fitline) #Summary of the regression line




