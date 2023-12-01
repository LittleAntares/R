q1.data <- read.table(file ="assignment2_prob1_2021.txt", header=TRUE)
names(data)
q1.data$prebake <- factor(q1.data$prebake)
q1.data$flux <- factor(q1.data$flux)
q1.data$cooling <- factor(q1.data$cooling)
q1.data$temp <- factor(q1.data$temp)

#Plotting Interaction plot
par(mfrow=c(2,3)) 
with(data=q1.data, interaction.plot(prebake, flux, numDefects)) 

with(data=q1.data, interaction.plot(prebake, cooling, numDefects))

with(data=q1.data, interaction.plot(prebake, temp, numDefects))

with(data=q1.data, interaction.plot(flux, cooling, numDefects))

with(data=q1.data, interaction.plot(flux, temp, numDefects))

with(data=q1.data, interaction.plot(cooling, temp, numDefects))







#Creating Addative model
Amodel <- glm(numDefects~., family = "poisson", data = q1.data)
summary(Amodel)

Fullmodel <- glm(numDefects~ (prebake+flux+cooling+temp)^2, family = "poisson", data=q1.data)
summary(Fullmodel)

model1 <- glm(numDefects~prebake*flux+prebake*cooling
              +prebake*temp+flux*temp+cooling*temp, 
              family = "poisson", data=q1.data)

anova(model1,Fullmodel, test="LRT")
# deviance residuals vs fitted values
plot(residuals(model1) ~ predict(model1,type="response"),
     xlab=expression(hat(lambda)), ylab="Deviance residuals")
par(mfrow=c(2,2)) 
# deviance residuals vs fitted values
plot(residuals(model1) ~ predict(model1,type="response"), xlab=expression(hat(lambda)), ylab="Deviance residuals")
# deviance residuals vs linear fitted values
plot(residuals(model1) ~ predict(model1),xlab=expression(hat(eta)), ylab="Deviance residuals")
# pearson residuals vs linear fitted values
plot(residuals(modp,type="pearson") ~ predict(model1), xlab=expression(hat(eta)), ylab="Pearson residuals")
# response residuals vs linear fitted values
plot(residuals(modp,type="response") ~ predict(model1), xlab=expression(hat(eta)), ylab="Response residuals")



par(mfrow=c(2,2)) 
halfnorm(influence(model1)$hat)
halfnorm(rstudent(model1)) 
halfnorm(cooks.distance(model1))
