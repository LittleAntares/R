
#Question 1:

#Entering 3 weeks of covid data
covid_data <- c(48,70,47,40,35,41,30,
                39,41,25,44,20,13,11,
                28,14,11,13,12,16,05)

#H0: m=m0 vs H1:m<15


#Conducting a wilcoxon to see if median cases is below 15
wilcox.test(covid_data, mu = 15, alternative = "less", exact = FALSE,
            correct = TRUE)



#Entering week 2 and week 3 data as a seperate vector
covid_week2 = c(39,41,25,44,20,13,11)
covid_week3 = c(28,14,11,13,12,16,05)

#H0: m2=m3 vs H1:m2>m3
#Conducting a wilcoxon sign test to compare median between week 2 and week 3
wilcox.test(covid_week2, covid_week3, alternative ="greater", exact = FALSE)
