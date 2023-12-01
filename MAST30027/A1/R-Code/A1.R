
#Q1.a Compute MLE using probit

#Loading Data
library(faraway)
data("orings")
str(orings)

#Define MLE_Function
MLE_f <- function(beta, orings) {
  eta <- cbind(1, orings$temp) %*% beta
  p <- pnorm(eta)
  return(sum( orings$damage*log(p/(1-p)) +6*log(1-p)))
}
#Calculating Beta.hat
(beta_hat <- optim(c(10,-.1),MLE_f, orings=orings, control = 
                     list(fnscale=-1))$par)


#Building Model using GLM
probitmodel <-  glm(cbind(damage,6-damage) ~ temp, family=binomial(link="probit"), orings) 
logitmodel <- probitmodel <-  glm(cbind(damage,6-damage) ~ temp, family=binomial, orings)
temp_rise <- seq(25, 85, 1)

#Getting sequence of response
logitresp <- predict(logitmodel,list(temp=temp_rise),type="response")
probitresp <- predict(probitmodel,list(temp=temp_rise), type = "response")

#Plotting
plot(temp_rise,logitresp, type = "l", col="red",xlab = "Temperature in Fahrenheit",
     ylab="Probability of Damage")
lines(temp_rise, probitresp, col="Blue")
