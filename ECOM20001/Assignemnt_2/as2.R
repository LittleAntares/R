
#Appendix 


#Pre Question

setwd("C:/Users/Kim CHY/Desktop/Assginment 2")  #Setting Working Dirctory
df <- read.csv(file="as2_billions.csv")         #Read File
options(scipen=0)                               #Preferred regular number over scientific notation


#Installing Stargazer Package
install.packages("stargazer")
install.packages("AER")

#loading the library
library(AER)
library(stargazer)


#Rescale variable to population to per million units
df$Mpop <- (df$pop/1000000)



#Question 1: Data Summary


#Getting Summary in R
summary(df)

#Creating a pre-formart table for LaTex Omitting  Number of observation
stargazer(df, title = "Data Summary of the World From 2005 to 2013", 
          omit.summary.stat = c("n"), digits= 2)



#Question 2: Top 20

#Creating sorted data of the first 20 observation in Working dirctory
write.csv(data.frame(df[order(-df$numbil), ][1:20, 1:12]), file = "df_sorted.csv")

#Assign the sorted data
df_sorted <- read.csv(file = "df_sorted.csv")

#Counting and Listing country and year in the new data set
table(df_sorted$country)
table(df_sorted$year)




#Question 3: Graph Ploting
#Plotting numbil vs gdppc with linear regression line and Remove Excess variables
y <- df$numbil
x <- df$gdppc

plot(x, y,
     main="Number of Billionaires and GDP per Capita",
     xlab="GDP per capita", ylab="Number of Billionaires",
     pch=19, las=1, col="blue", ylim = c(0,500), xlim = c(0,120000))
abline(lm(y ~ x, data=df), col="red", lwd=2)
remove(x, y)

#Plotting numbil vs gattwto08 with linear regression line and Remove Excess variables
y <- df$numbil
x <- df$gattwto08

plot(x, y,
     main="Number of Billionaires and Number of Year in GATT or WTO as of 2008",
     xlab="Number of Year in GATT or WTO as of 2008", ylab="Number of Billionaires",
     pch=19, las=1, col="blue",  ylim = c(0,500))
abline(lm(y ~ x, data=df), col="red", lwd=2)
remove(x, y)


#plotting gdppc vs gattawto with Linear Regression line and Remove Excess variables
y <- df$gdppc
x <- df$gattwto08

plot(x, y,
     main="GPD per Capita and Number of Year in GATT or WTO as of 2008",
     xlab="Number of Year in GATT or WTO as of 2008", ylab="GPD per Capita",
     pch=19, col="blue", ylim = c(0,120000))
abline(lm(y ~ x, data=df), col="red", lwd=2)
remove(x, y)


#All Image are exported Manually


#Question 4:

reg_numbil_gdppc <- lm(df$numbil~df$gdppc, data = df)
summary(reg_numbil_gdppc)


#Question 5



#Create Dummy variable for 2005 to 2013 with Column Name 1 to 9 temporary
number = c(2005:2013)
df <- cbind( df , sapply( number , function(x) as.numeric( df$year == x ) ) )

#Rename the Column Name to d2005 to d2013
new_year = c("d2005","d2006","d2007","d2008","d2009","d2010","d2011","d2012","d2013")
colnames(df)[13:21] <- new_year

#To Run the code above sucessfully the Column index must be correct. In this case, no adjustment to the existing data frame

#Remove Excess variables
remove(number, new_year)



#create a constant data in the data frame with the sum of d2005 to d2013 for each row temporary
df$d_constant <- df$d2005+df$d2006+df$d2007+df$d2008+df$d2009+df$d2011+df$d2010+df$d2012+df$d2013
summary(df$d_constant)

#Creating regression with the constant and printing the regression result
reg_constant <- lm(df$numbil~df$d_constant, data=df)
summary(reg_constant)

#Creating regression with d2005 to d2013 as separatedummy variable the and printing the regression result
reg_d2005_to_d2013 <- lm(df$numbil~df$d2005+df$d2006+df$d2007+df$d2008+df$d2009+df$d2010+df$d2011+df$d2012, data=df)
summary(reg_d2005_to_d2013)



#Question 6:

# Creating Reg (1) with following variable: pop
reg_1 <- lm(numbil~Mpop , data=df)

#Creating Reg (2) with following variables: pop, gdppc
reg_2 <- lm(numbil~Mpop+gdppc , data=df)

#Creating Reg (3) with following variable: pop, gdppc, gattwto08
reg_3 <- lm(numbil~Mpop+gdppc+gattwto08, data=df)

#Creating Reg (4) with following variable: pop, gdppc, gattwto08, fullprivproc
reg_4 <- lm(numbil~Mpop+gdppc+gattwto08+fullprivproc, data=df)

#Creating Reg (5) with following variable: pop, gdppc, gattwto08, fullprivproc, topint08
reg_5 <- lm(numbil~Mpop+gdppc+gattwto08+fullprivproc+topint08, data=df)

#Creating Reg (6) with following variable: pop, gdppc, gattwto08, fullprivproc, topint08, rint
reg_6 <- lm(numbil~Mpop+gdppc+gattwto08+fullprivproc+topint08+rintr, data=df)

#Creating Reg (7) with following variable: pop, gdppc, gattwto08, fullprivproc, topint08, rint, roflaw, nrrents
reg_7 <- lm(numbil~Mpop+gdppc+gattwto08+fullprivproc+topint08+rintr+roflaw+nrrents, data=df)

#Creating Reg (8) with following variable: pop, gdppc, gattwto08, fullprivproc, topint08, rint, roflaw, nrrents,d2006, d2007, d2008, d2009, d2010, d2011, d2012, d2013
reg_8 <- lm(numbil~Mpop+gdppc+gattwto08+fullprivproc+topint08+rintr+roflaw+nrrents+d2005+d2006+d2007+d2008+d2009+d2010+d2011+d2012, data=df)




#Saved the robust standard errors for reg(1)
cov_1=vcovHC(reg_1, type = "HC1")           
se_1=sqrt(diag(cov_1))

#Saved the robust standard errors for reg(2)
cov_2=vcovHC(reg_2, type = "HC1")           
se_2=sqrt(diag(cov_2))

#Saved the robust standard errors for reg(3)
cov_3=vcovHC(reg_3, type = "HC1")           
se_3=sqrt(diag(cov_3))

#Saved the robust standard errors for reg(4)
cov_4=vcovHC(reg_4, type = "HC1")           
se_4=sqrt(diag(cov_4))


#Saved the robust standard errors for reg(5)
cov_5=vcovHC(reg_5, type = "HC1")           
se_5=sqrt(diag(cov_5))


#Saved the robust standard errors for reg(6)
cov_6=vcovHC(reg_6, type = "HC1")           
se_6=sqrt(diag(cov_6))

#Saved the robust standard errors for reg(7)
cov_7=vcovHC(reg_7, type = "HC1")           
se_7=sqrt(diag(cov_7))

#Saved the robust standard errors for reg(8)
cov_8=vcovHC(reg_8, type = "HC1")           
se_8=sqrt(diag(cov_8))


#Producing Table for Reg (1) and Reg(2) in LaTex
stargazer(reg_1, reg_2,
          digits=4, se=list(se_1, se_2),
          column.labels = c("Reg(1)", "Reg(2)"),
          dep.var.labels=c("Billionaire in a Country From 2005 to 2013"),
          covariate.labels=
            c("Population per Million",
              "GDP per capita"))


#Producing Table For Reg(3) to Reg(6) in LaTex

stargazer(reg_3, reg_4, reg_5, reg_6,
          digits=4, se=list(se_3, se_4, se_5, se_6),
          column.labels = c("Reg(3)", "Reg(4)", "Reg(5)", "Reg(6)"),
          dep.var.labels=c("Billionaire in a Country From 2005 to 2013"),
          covariate.labels=
            c("Population per Million",
              "GDP per capita",
              "Number of Year in GATT or WTO as 2008",
              "Privatisation Proceed in the economy",
              "Top Marginal income Tax as 2008",
              "Real Interest Rate as 2008",
              "Rule of Law Index as 2008",
              "Natural Resources Rents as % in GDP in 2008"))


#Producing Table For Reg(7) and Reg(8) in LaTex
stargazer(reg_7, reg_8,
          digits=4, se=list(se_7, se_8),
          column.labels = c("Reg(7)", "Reg(8)"),
          dep.var.labels=c("Billionaire in a Country From 2005 to 2013"),
          covariate.labels=
            c("Population Per Million",
              "GDP per capita",
              "Number of Year in GATT or WTO as 2008",
              "Privatisation Proceed in the economy",
              "Top Marginal income Tax as 2008",
              "Real Interest Rate as 2008",
              "Rule of Law Index as 2008",
              "Natural Resources Rents as % in GDP in 2008",
              "Year 2005",
              "Year 2006",
              "Year 2007",
              "Year 2008",
              "Year 2009",
              "Year 2010",
              "Year 2011",
              "Year 2012"))


#Question 7:

#Producing Regression table in text and output them in a file name regression.txt for Reg(1) to Reg(8)
stargazer(reg_1, reg_2,reg_3, reg_4, reg_5, reg_6,reg_7, reg_8, type = "text",
          digits=6, se=list(se_1, se_2, se_3, se_4, se_5, se_6, se_7, se_8),
          column.labels = c("Reg(1)","Reg(2)","Reg(3)","Reg(4)","Reg(5)","Reg(6)","Reg(7)", "Reg(8)"),
          dep.var.labels=c("Billionaire in a Country From 2005 to 2013"),
          covariate.labels=
            c("Population per Million",
              "GDP per capita",
              "Number of Year in GATT or WTO as 2008",
              "Privatisation Proceed in the economy",
              "Top Marginal income Tax as 2008",
              "Real Interest Rate as 2008",
              "Rule of Law Index as 2008",
              "Natural Resources Rents as % in GDP in 2008",
              "Year 2005",
              "Year 2006",
              "Year 2007",
              "Year 2008",
              "Year 2009",
              "Year 2010",
              "Year 2011",
              "Year 2012",
              "Year 2013"), out = "regression.txt")


# Printing reg(8) results with robust standard errors
coeftest(reg_8, vcov = vcovHC(reg_8, "HC0"))

#Printing the current data summary
stargazer(df, type="text")




#Question 10

#Testing for H0:B_roflaw=0 and B_nrrents=0, H0:B_roflaw =! 0 or B_nrrents =! 0
linearHypothesis(reg_8,c("roflaw=0","nrrents=0"),vcov = vcovHC(reg_8, "HC1"))
summary(reg_8)

#Testing for H0:B_topint08 = B_rintr, H1: B_topint08 =! B_rintr
linearHypothesis(reg_8,c("topint08=rintr"),vcov = vcovHC(reg_8, "HC1"))
summary(reg_8)
