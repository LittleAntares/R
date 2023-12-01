#Pre Question
data=read.csv(file="as1_wine.csv") #open file


#defining data$name to a simple varible
price=(data$price)
cases=(data$cases)
score=(data$score)
napa=(data$napa)
sonoma=(data$sonoma)
d1995=(data$d1995)
d1999=(data$d1999)

#function for calucating the mean different between two data set
df_mean <- function(x,y){
  paste( mean(x), "-", mean(y),"=", abs(mean(x)-mean(y)))
}


# Q1.(3 marks) Report summary statistics for price, cases, score, napa, sonoma,d1995, d1999. Interpret each of the means in plain language, thereby characterising a typical wine in the dataset. Your answer should be no more than four sentences long.

#getting summary of data
summary(data) #Printing out summary data

#Getting Standard Deviation
sd(price)  #Standard Deviation for Price
sd(cases)  #Standard Deviation for Cases
sd(score)  #Standard Deviation for Score
sd(napa)   #Standard Deviation for Napa
sd(sonoma) #Standard Deviation for Sonoma
sd(d1995)  #Standard Deviation for d1995
sd(d1999)  #Standard Deviation for d1999


#Q2.(3 marks) Compute 99% confidence intervals for price, cases, score

#computing 99% confidence interval for price
x=mean(price)        # Sample mean of price
n=length(price)      # Number of sample(N) 
std=sd(price)        # Sample standard deviation
m=std/sqrt(n)        # Marginal error of the sample mean
CI99_lower=x-2.58*m  # Lower bound of the 99% CI
CI99_upper=x+2.58*m  # Upper bound of the 99% CI


#Printing out 99% confidence interval result for price
paste("99% confidence interval for prices", "(" ,CI99_lower,  ",",CI99_upper, ")" )
#remove excess values
remove(x, n, std, m, CI99_upper, CI99_lower)


#computing 99% confidence interval for price
x=mean(cases)        # Sample mean of price
n=length(cases)      # Number of sample(N) 
std=sd(cases)        # Sample standard deviation
m=std/sqrt(n)        # Marginal error of the sample mean
CI99_lower=x-2.58*m  # Lower bound of the 99% CI
CI99_upper=x+2.58*m  # Upper bound of the 99% CI


#Printing out 99% confidence interval result for cases
paste("99% confidence interval for cases", "(" ,CI99_lower,  ",",CI99_upper, ")" )
#remove excess values
remove(x, n, std, m, CI99_upper, CI99_lower)


#computing 99% confidence interval for score
x=mean(score)        # Sample mean of price
n=length(score)      # Number of sample(N) 
std=sd(score)        # Sample standard deviation
m=std/sqrt(n)        # Marginal error of the sample mean
CI99_lower=x-2.58*m  # Lower bound of the 99% CI
CI99_upper=x+2.58*m  # Upper bound of the 99% CI


#Printing out 99% confidence interval result for price
paste("99% confidence interval for score", "(" ,CI99_lower,  ",",CI99_upper, ")" )
#remove excess values
remove(x, n, std, m, CI99_upper, CI99_lower)


#3. Graphing different between the napa wine region and sonoma wine region

#Plotting graph for prices
skew
plot(density(price[sonoma==1]), col="blue",lty=1,
     main="Price different between a bottle of wine in Napa region and Sonoma region", 
     xlab="Price($)",las=1)
lines(density(price[napa==1]), col="red", lty=1)
legend("topright", legend=c("Napa region", "Sonoma Region"), 
       col=c("red","blue"), lty=c(1,1))


#Plotting graph for cases
plot(density(cases[sonoma==1]), col="blue",lty=1,
     main="Production different between Napa region and Sonoma region", 
     xlab="Case", las=1)
lines(density(cases[napa==1]), col="red", lty=1)
legend("topright", legend=c("Napa region", "Sonoma Region"), 
       col=c("red","blue"), lty=c(1,1))

#Plotting graph for score
plot(density(score[sonoma==1]), col="blue",lty=1,
     main="Tasting score between a bottle of wine in Napa region and Sonoma region", 
     xlab="WSM tasting score(out of 100)", las=1)
lines(density(score[napa==1]), col="red", lty=1)
legend("topright", legend=c("Napa region", "Sonoma Region"), 
       col=c("red","blue"), lty=c(1,1))

#4 Graphing the different between wine price and quality produced 1995 and 1999

#Plotting graph for prices in napa valley between 1995 and 1999
plot(density(price[napa==1  & d1995==1]), col="blue",lty=1,main="Price different in a bottle of wine in Napa region between 1995 and 1999 ", xlab="Price($)")
lines(density(price[napa==1 & d1999==1]), col="red", lty=1)
legend("topright", legend=c("1995", "1999"), 
       col=c("blue","red"), lty=c(1,1))

#Plotitng graph for prices in napa valley between 1995 and 1999
plot(density(score[napa==1  & d1995==1]), col="blue",lty=1,
     main="Tasting score different in a bottle of wine in Napa region between 1995 and 1999 ", 
     xlab="WSM tasting score(out of 100)", las=1)
lines(density(score[napa==1 & d1999==1]), col="red", lty=1)
legend("topright", legend=c("1995", "1999"), 
       col=c("blue","red"), lty=c(1,1))

#Plotting graph for prices in sonoma valley between 1995 and 1999
plot(density(price[sonoma==1  & d1995==1]), col="blue",lty=1,
     main="Price different in a bottle of wine in Sonoma region between 1995 and 1999 ", 
     xlab="Price($)", las=1)
lines(density(price[sonoma==1 & d1999==1]), col="red", lty=1)
legend("topright", legend=c("1995", "1999"), 
       col=c("blue","red"), lty=c(1,1))

#Plotting graph for score in sonoma valley between 1995 and 1999
plot(density(score[sonoma==1  & d1999==1]), col="red",lty=1, 
     main="Tasting score different in a bottle of wine in Sonoma region between 1995 and 1999 ", 
     xlab="WSM tasting score(out of 100)", las=1)
lines(density(score[sonoma==1 & d1995==1]), col="blue", lty=1)
legend("topright", legend=c("1995", "1999"), 
       col=c("blue","red"), lty=c(1,1))


#5 Plotting scatter plot

# score on the vertal axis and price on the horizontal axis
x = price
y = score

plot(x, y,
     main="Relationship between Price and WSM tasting score Scatter Plot",
     xlab= "Price($)", ylab="WSM tasting score",
     pch=19, las=1, ylim=c(65,100), col="blue")
grid(nx=30, ny=7, col="lightgray", lty = "dotted")
abline(lm(y ~ x, data = data), col = "red", lwd=1)
paste("Covavriance=", cov(x,y))
paste("Correlation=", cor(x,y))
remove(x, y)

summary(test)

# score on the vertal axis and cases on the horizontal axis
x = cases
y = score

plot(x, y,
     main="Relationship between Cases and WSM tasting score Scatter Plots",
     xlab= "Cases", ylab="WSM tasting score",
     pch=19, las=1, ylim=c(65,100), col="blue")
grid(nx=30, ny=7, col="lightgray", lty = "dotted")
abline(lm(y ~ x, data = data), col = "red", lwd=1)
paste("Covavriance=", cov(x,y))
paste("Correlation=", cor(x,y))
remove(x, y)


#6 Testing the following

#Test 6a T-test mean price different between napa and overall price
t1=price[napa==1]
t2=price[napa==0]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#Test 6b T-test mean score different between napa overall score
t1=score[napa==1]
t2=score[napa==0]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#Test 6c T-test mean price different between sonoma and overall price
t1=price[sonoma==1]
t2=price[sonoma==0]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#Test 6d T-test average score different between sonoma and overall score
t1=score[sonoma==1]
t2=score[sonoma==0]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#7 Testing the following


#7a T test for Nappa mean price different between 1995 and 1999
t1=price[napa==1 & d1999==1]
t2=price[napa==1 & d1995==1]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#7b T-test for Nappa mean score different between 1995 and 1999
t1=score[napa==1 & d1999==1]
t2=score[napa==1 & d1995==1]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#7c T-test for Sonoma mean price different between 1995 and 1999
t1=price[sonoma==1 & d1999==1]
t2=price[sonoma==1 & d1995==1]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible

#7d T-test for sonoma mean score different between 1995 and 1999
t1=score[sonoma==1 & d1999==1]
t2=score[sonoma==1 & d1995==1]
df_mean(t1, t2)    #Calculating the mean different using user-made defined previously
t.test(t1, t2)     #two tailed t-test for different in sample mean of 0
remove(t1, t2)     #remove excess varible



