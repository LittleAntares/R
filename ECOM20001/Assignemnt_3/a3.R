
#Appendix


#Pre-Question
setwd("C:/Users/Kim CHY/Desktop/Assignment 3") #set working directory

df <- read.csv(file = "as3_smoke.csv")         #opening file


#installing packages
install.packages("Stargazer")
install.packages("AER")
install.packages("ggplot2")

#load-packages
library(AER)
library(stargazer)
library(ggplot2)

#Question 1:regression table

View(df)  #viewing data set
names(df) #listing data frame

df$log_brithweight <- log(df$birthweight) #Defining Birhtweight

#Creating and saved robust standard errors for reg(1):smoker
reg_1 <- lm(log_brithweight~smoker, data = df)
cov_1=vcovHC(reg_1, type = "HC1")           
se_1=sqrt(diag(cov_1))

#Creating and saved robut standard errors for reg(2):Smoker, alcohol, drinks, gambles
reg_2 <- lm(log_brithweight~smoker+alcohol+drinks+gambles, data = df)
cov_2=vcovHC(reg_2, type = "HC1")           
se_2=sqrt(diag(cov_2))

#Creating and saved robut standard errors for reg(3):Smoker, alcohol, drinks, gambles,nprevisit,tripre1,tripre2,tripre3
reg_3 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+nprevisit+tripre1+tripre2+tripre3, data = df)
cov_3=vcovHC(reg_3, type = "HC1")           
se_3=sqrt(diag(cov_3))

#Creating and saved robut standard errors for reg(4):Smoker, alcohol, drinks, gambles,nprevisit,tripre1,tripre2,tripre3,unmarried,educ,age
reg_4 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age, data = df)
cov_4=vcovHC(reg_4, type = "HC1")           
se_4=sqrt(diag(cov_4))


#Producing a regression table for Latex
stargazer(reg_1,reg_2,reg_3,reg_4,
          digits = 4, se=list(se_1, se_2, se_3, se_4),
          column.labels = c("Reg(1)", "Reg(2)", "Reg(3)", "Reg(4)"),
          dep.var.labels=c("Birthweight approximate changes in %"),
          covariate.labels=
            c("Smoker",
              "Drinks Alochol during Pregnancy",
              "Drinks per Weeks during Pregnancy",
              "Gambling Problem",
              "Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age of Mother"),out = "regression.txt")

#Producing a regression table for personal viewing
stargazer(reg_1,reg_2,reg_3,reg_4, type = "text",
          digits = 4, se=list(se_1, se_2, se_3, se_4),
          column.labels = c("Reg(1)", "Reg(2)", "Reg(3)", "Reg(4)"),
          dep.var.labels=c("Birthweight approximate changes in %"),
          covariate.labels=
            c("Smoker",
              "Drinks Alochol during Pregnancy",
              "Drinks per Weeks during Pregnancy",
              "Gambling Problem",
              "Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age of Mother"),out = "regression.txt")

#Question 2: Scatter Plot

#Plotting Scatter Plot
ggplot(df, aes(y=log_brithweight, x=nprevisit)) +                                         # Define the dataset, x and y variables for scatter plot
  geom_point(alpha = .3) +                                                                # Allow for shading of the points in the scatter plot to help visualisation
  stat_smooth(method = "lm", formula = y ~ poly(x,2), col="blue") +                       # Fit a polynomial of DEGREE 2 (QUADRATIC)
  ggtitle("Relationship Between Log of Birthweight and Total Number of Prenatal Visit") +      # Scatter plot title
  theme(plot.title = element_text(hjust = 0.5)) +                       # Center the scatter plot title
  scale_x_continuous(name="Total Number of Prenatal Visit", limits=c(0, 35),breaks=seq(0,35,5)) +                              # x-axis title, limits, lines
  scale_y_continuous(name="Log of Birthweight", limits=c(6, 9),breaks=seq(6,9,0.5))

#Question 3: Reg(5)

df$nprevisit_sq <- df$nprevisit^2 #Define nprevisit square

#Creating and saved robut standard errors for reg(5):Smoker, alcohol, drinks, gambles,nprevisit,tripre1,tripre2,tripre3,unmarried,educ,age,nprevisit_sq
reg_5 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age+nprevisit_sq, data = df)
cov_5=vcovHC(reg_5, type = "HC1")           
se_5=sqrt(diag(cov_5))


#Producing Regression Table
stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,
          digits = 4, se=list(se_1, se_2, se_3, se_4, se_5),
          column.labels = c("Reg(1)", "Reg(2)", "Reg(3)", "Reg(4)"),
          dep.var.labels=c("Birthweight approximate changes in %"),
          covariate.labels=
            c("Smoker",
              "Drinks Alochol during Pregnancy",
              "Drinks per Weeks during Pregnancy",
              "Gambling Problem",
              "Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age of Mother",
              "Prenatal Visits Square"),out = "regression.txt")


#Question 4: Partial Effect
names(df)

#Partial Different between 2 to 4 visits
dfnew2 <- data.frame(nprevisit=2,nprevisit_sq=2^2,smoker=0,alcohol=0, drinks=0,    #Creating New Data Frames 2 visits holding other regressor at 0
                     tripre1=0, tripre2=0,tripre3=0, tripre0=0,
                     unmarried=0, educ=0, age=0, gambles=0)

dfnew4 <- data.frame(nprevisit=4,nprevisit_sq=4^2,smoker=0,alcohol=0, drinks=0,    #Creating New Data Frames 4 visits holding other regressor at 0
                     tripre1=0, tripre2=0,tripre3=0, tripre0=0,
                     unmarried=0, educ=0, age=0, gambles=0)

bw2 <- predict(reg_5, newdata = dfnew2)
bw4 <- predict(reg_5, newdata = dfnew4)
pebw2_4 <- bw4-bw2

# Compute F-stat for 2 to 4 visits and error
Ftest2_4 <- linearHypothesis(reg_5,c("2*nprevisit+12*nprevisit_sq=0"),vcov = vcovHC(reg_5, "HC1"))
Fstat2_4 <- Ftest2_4[2,3]
se_pebw2_4=abs(pebw2_4)/sqrt(Fstat2_4)

#Computing 95 CI for 2 to 4
pebw2_4_ci95L=pebw2_4-se_pebw2_4*1.96
pebw2_4_ci95H=pebw2_4+se_pebw2_4*1.96
pebw2_4_range=pebw2_4_ci95H-pebw2_4_ci95L

## Outputting results
sprintf("partial effect: %f", pebw2_4)
sprintf("SE of partial effect: %f", se_pebw2_4)
sprintf("95 CI lower bound for partial effect: %f", pebw2_4_ci95L)
sprintf("95 CI upper bound for partial effect: %f", pebw2_4_ci95H)
sprintf("Width of 95 CI for partial effect: %f", pebw2_4_range)



#Partial Different between 12 to 14 visits
dfnew12 <- data.frame(nprevisit=12,nprevisit_sq=12^2,smoker=0,alcohol=0, drinks=0,    #Creating New Data Frames 12 visits holding other regressor at 0
                     tripre1=0, tripre2=0,tripre3=0, tripre0=0,
                     unmarried=0, educ=0, age=0, gambles=0)

dfnew14 <- data.frame(nprevisit=14,nprevisit_sq=14^2,smoker=0,alcohol=0, drinks=0,    #Creating New Data Frames 14 visits holding other regressor at 0
                     tripre1=0, tripre2=0,tripre3=0, tripre0=0,
                     unmarried=0, educ=0, age=0, gambles=0)

bw12 <- predict(reg_5, newdata = dfnew12)
bw14 <- predict(reg_5, newdata = dfnew14)
pebw12_14 <- bw14-bw12

# Compute F-stat for 2 to 4 visits and error
Ftest12_14 <- linearHypothesis(reg_5,c("2*nprevisit+52*nprevisit_sq=0"),vcov = vcovHC(reg_5, "HC1"))
Fstat12_14 <- Ftest12_14[2,3]
se_pebw12_14=abs(pebw12_14)/sqrt(Fstat12_14)

#Computing 95 CI from 12 to 14
pebw12_14_ci95L=pebw12_14-se_pebw12_14*1.96
pebw12_14_ci95H=pebw12_14+se_pebw12_14*1.96
pebw12_14_range=pebw12_14_ci95H-pebw12_14_ci95L






## Outputting results
sprintf("partial effect: %f", pebw12_14)
sprintf("SE of partial effect: %f", se_pebw12_14)
sprintf("95 CI lower bound for partial effect: %f", pebw12_14_ci95L)
sprintf("95 CI upper bound for partial effect: %f", pebw12_14_ci95H)
sprintf("Range of 95 CI for partial effect: %f", pebw12_14_range)

#Question 5: Log_brithweight vs log_nprevisit scatter plot

df$log_nprevisit <- log(1+df$nprevisit)

#Plotting Scatter Plot
ggplot(df, aes(y=log_brithweight, x=log_nprevisit)) +                                         # Define the dataset, x and y variables for scatter plot
  geom_point(alpha = .3) +                                                                # Allow for shading of the points in the scatter plot to help visualisation
  stat_smooth(method = "lm", formula = y ~ poly(x,2), col="blue") +                       # Fit a polynomial of DEGREE 2 (QUADRATIC)
  ggtitle("Relationship Between Log(Birthweight) and Log(1+Nprevisit)") +      # Scatter plot title
  theme(plot.title = element_text(hjust = 0.5)) +                       # Center the scatter plot title
  scale_x_continuous(name="Log of 1 + Total Number of Prenatal Visit", limits=c(0, 4),breaks=seq(0,4,0.5)) +                              # x-axis title, limits, lines
  scale_y_continuous(name="Log of Birthweight", limits=c(6, 9),breaks=seq(6,9,0.5)) 


#Question 6:


df$log_nprevisit_sq <- df$log_nprevisit^2  #Creating log_nprevisit_sq


#Reg(1) to Reg(2) is the same is regression in question 1 so no edit

#Creating and saved robut standard errors for reg(3):Smoker, alcohol, drinks, gambles,nprevisit,tripre1,tripre2,tripre3
reg_new_3 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+log_nprevisit+tripre1+tripre2+tripre3, data = df)
cov_new_3=vcovHC(reg_new_3, type = "HC1")           
se_new_3=sqrt(diag(cov_new_3))

#Creating and saved robut standard errors for reg(4):Smoker, alcohol, drinks, gambles,log_nprevisit,tripre1,tripre2,tripre3,unmarried,educ,age
reg_new_4 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+log_nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age, data = df)
cov_new_4=vcovHC(reg_new_4, type = "HC1")           
se_new_4=sqrt(diag(cov_new_4))

#Creating and saved robut standard errors for reg(5):Smoker, alcohol, drinks, gambles,nprevisit,tripre1,tripre2,tripre3,unmarried,educ,age,nprevisit_sq
reg_new_5 <- lm(log_brithweight~smoker+alcohol+drinks+gambles+log_nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age+log_nprevisit_sq, data = df)
cov_new_5=vcovHC(reg_new_5, type = "HC1")           
se_new_5=sqrt(diag(cov_new_5))

#Producing New regression Table
stargazer(reg_1,reg_2,reg_new_3,reg_new_4,reg_new_5,
          digits = 4, se=list(se_1, se_2, se_new_3, se_new_4, se_new_5),
          column.labels = c("Reg(1)", "Reg(2)", "Reg(3)", "Reg(4)"),
          dep.var.labels=c("Log of Birthweight"),
          covariate.labels=
            c("Smoker",
              "Drinks Alochol during Pregnancy",
              "Drinks per Weeks during Pregnancy",
              "Gambling Problem",
              "Log of Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age of Mother",
              "Log of Prenatal Visits Square"),out = "new_regression.txt")


#Question 7: Regression 6


df$log_nprevisit_age = df$log_nprevisit*df$age #defining log_previsit_age

#Creating Regression 6 and  saved robut standard errors
reg_6 <-  lm(log_brithweight~smoker+alcohol+drinks+gambles+log_nprevisit+tripre1+tripre2+tripre3+unmarried+educ+age+log_nprevisit_age, data = df)
cov_6=vcovHC(reg_6, type = "HC1")           
se_6=sqrt(diag(cov_6))

summary(reg_6)

#Producing New regression Table
stargazer(reg_1,reg_2,reg_new_3,reg_new_4,reg_new_5,reg_6,
          digits = 4, se=list(se_1, se_2, se_new_3, se_new_4, se_new_5,se_6),
          column.labels = c("Reg(1)", "Reg(2)", "Reg(3)", "Reg(4)", "Reg(5)","Reg(6)"),
          dep.var.labels=c("Log of Birthweight"),
          covariate.labels=
            c("Smoker",
              "Drinks Alochol during Pregnancy",
              "Drinks per Weeks during Pregnancy",
              "Gambling Problem",
              "Log (Total Number of Prenatal Visits)",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Unmarried",
              "Years of Education",
              "Age of Mother",
              "Log (Prenatal Visits Square)",
              "Log (Prenatal Visits) X Age "),out = "new_regression.txt")

#Question 8: Elasticity

summary(reg_6)    #getting regression 6 summary



#Elasticity for bithweight with respect to nprevisit Calucation for age 20 mother
e20 <- summary(reg_6)$coefficients[6,1]+summary(reg_6)$coefficients[13,1]*20


# Regression F-test, working out B5+20B6 can be seen on the report
Ftest_e20 <- linearHypothesis(reg_6,c("log_nprevisit+20*log_nprevisit_age=0"),vcov = vcovHC(reg_6, "HC1"))

## Recover the Fstat from the joint test results in Ftest
Fstat_e20 <- Ftest_e20[2,3]
sprintf("Fstat %f", Fstat_e20)
## Compute the standard error for the Elasticity we computed, dahe (see slide 21 of Lecture note 8) 
se_e20 <- abs(e20)/sqrt(Fstat_e20)

## 95% CI for the Elasticity we computet, for age = 20
e20_ci95L <- e20-se_e20*1.96
e20_ci95H <- e20+se_e20*1.96
e20_range <- e20_ci95H-e20_ci95L

## Outputting results for age = 20
sprintf("Elasticity: %f", e20)
sprintf("SE of Elasticity: %f", se_e20)
sprintf("95 CI lower bound for Elasticity: %f", e20_ci95L)
sprintf("95 CI upper bound for Elasticity: %f", e20_ci95H)
sprintf("Range of 95 CI for Elasticity: %f", e20_range)

#Complied Output into a data frame and producing a table for Latex and in text
e20_data <- data.frame(Elasticity=e20*100, F_stat = Fstat_e20, SE_of_Elasticity=se_e20*100, 
                       CI_95_Lower_Bound_for_Elasticity=e20_ci95L*100, 
                       CI_95_Upper_Bound_for_Elasticity= e20_ci95H*100, 
                       CI_95_Range_for_Elasticity = e20_range*100)

stargazer(e20_data, digits = 2, out = "e20.txt",
          omit.summary.stat = c("max", "median","min","n","p25","p75","sd"))  





#Elasticity for bithweight with respect to nprevisit Calucation for age 40 mother
e40 <- summary(reg_6)$coefficients[6,1]+summary(reg_6)$coefficients[13,1]*40


# Regression F-test, working out B5+40B6 can be seen on the report
Ftest_e40 <- linearHypothesis(reg_6,c("log_nprevisit+40*log_nprevisit_age=0"),vcov = vcovHC(reg_6, "HC1"))

## Recover the Fstat from the joint test results in Ftest
Fstat_e40 <- Ftest_e40[2,3]
sprintf("Fstat %f", Fstat_e40)
## Compute the standard error for the Elasticity we computed
se_e40 <- abs(e40)/sqrt(Fstat_e40)

## 95% CI for the Elasticity we computet, for age = 40
e40_ci95L <- e40-se_e40*1.96
e40_ci95H <- e40+se_e40*1.96
e40_range <- e40_ci95H-e40_ci95L

## Outputting results for age = 40
sprintf("Elasticity: %f", e40)
sprintf("SE of Elasticity: %f", se_e40)
sprintf("95 CI lower bound for Elasticity: %f", e40_ci95L)
sprintf("95 CI upper bound for Elasticity: %f", e40_ci95H)
sprintf("Range of 95 CI for Elasticity: %f", e40_range)

#Complied Output into a data frame and producing a table for Latex and in text
e40_data <- data.frame(Elasticity=e40*100, F_stat = Fstat_e40, SE_of_Elasticity=se_e40*100, 
                       CI_95_Lower_Bound_for_Elasticity=e40_ci95L*100, 
                       CI_95_Upper_Bound_for_Elasticity= e40_ci95H*100, 
                       CI_95_Range_for_Elasticity = e40_range*100)

stargazer(e40_data, digits = 2, out = "e40.txt",
          omit.summary.stat = c("max", "median","min","n","p25","p75","sd")) 

