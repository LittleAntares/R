
setwd("C:/Users/ksean/OneDrive/Desktop/Q2 2023/Cost_Class")

library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(openxlsx)

###################################################################


Compute_cost <- function(df_in,df_cost){
  n1 = nrow(df_in)
  n2 = nrow(df_cost)
  df_in$PurchasePrice = 0
  
  df_in$PurchasePrice = 0
  for(i in 1:n1){
    for(j in 1:n2){
      if(df_in$Child.SKU[i] == df_cost$ItemNumber[j]){
        df_in$PurchasePrice[i] = df_cost$PurchasePrice[j]
        break
      }
    }
  }
  return(df_in)
}



#################################################################


inv_comp <- read.csv(file = "Inv_Comp.csv", header = TRUE)
cost_info <- read.csv(file = "RAP-SOH StockValue Purchase Price for Default Location.csv",header = TRUE)

inv_comp <- inv_comp[order(inv_comp$Parent.SKU, decreasing = FALSE), ]
inv_comp = Compute_cost(inv_comp,cost_info)

write.csv(inv_comp, file = "Inv_Data.csv", row.names = FALSE)