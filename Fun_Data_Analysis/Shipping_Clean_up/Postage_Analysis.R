

#Load Library
library(tidyr)
library(dplyr)
library(stringi)
library(stringr)
library(openxlsx)

setwd("C:/Users/ksean/OneDrive/Desktop/Q2 2023")
################################################################################

#Create New AusPost Data with the required Header
Create_New <- function(df_in,header){
  
  data_header = df_in[1,]
  n1 = length(data_header)
  n2  = 0
  for(i in 1:length(header)){
    if(header[i] != ""){
      n2 = n2+1
    }
  }
  df_out <- data.frame(matrix(nrow = nrow(df_in) - 1 ,ncol = n2))
  colnames(df_out) = header[1:n2]
  
  for(i in 1:n1){
    for(j in 1:n2){
      name = header[j]
      if(name == data_header[i]){
        df_out[,j] = df_in[2:nrow(df_in),i]
        break
      }
    }
  }
  df_out <- df_out[!is.na(df_out[,1]),]
  return(df_out)
}


#Classify Destination Zone
Class_Zone <- function(df_in,zone){
  n1 <- nrow(df_in)
  n2 <- nrow(zone)
  
  
  for(i in 1:n1){
    for(j in 1:n2){
      if(as.numeric(df_in$Postcode[i]) == as.numeric(zone$Post.Code[j])){
        df_in$`Dest Zone`[i] = zone$Destination.zone[j]
        break
      }
    }
  }
  return(df_in)
}

Class_AP_Service <- function(df_in,service){
  n1 <- nrow(df_in)
  n2 <- nrow(service)
  df_in$MP_Cost = 0
  df_in$DCW = 0
  
  for(i in 1:n1){
    for(j in 1:n2){
      x1 = as.character(df_in$`Reference 1`[i])
      x2 = as.character(service$Reference.1[j])
      if(is.na(x2)){
        if(df_in$`Consignment No.`[i] == service$Carrier.Consignment..[j]){
          df_in$`Type of Service`[i] = service$Service[j]
          df_in$`Declared Weight`[i] = service$Total.Weight[j]
          df_in$`Declared Cubic Weight`[i] = Compute_Cubic(service$Total.Volume[j])
          df_in$DCW[i] = Compute_weight(df_in$`Declared Weight`[i],
                                        df_in$`Declared Cubic Weight`[i])
          df_in$MP_Cost[i] = service$Total[j]
          break 
        }
      } else if(x1 == x2){
        df_in$`Type of Service`[i] = service$Service[j]
        df_in$`Declared Weight`[i] = service$Total.Weight[j]
        df_in$`Declared Cubic Weight`[i] = Compute_Cubic(service$Total.Volume[j])
        df_in$DCW[i] = Compute_weight(df_in$`Declared Weight`[i],
                                      df_in$`Declared Cubic Weight`[i])
        df_in$MP_Cost[i] = service$Total[j]
        break
      }
    }
  }
  return(df_in)
}

Compare_Weight <- function(x1,x2){
  cat = c("Same","True","False")
  if(x1 == x2) return(cat[1])
  if(x1 < x2) return(cat[2])
  if(x1 > x2) return(cat[3])
}

Clean_Weight <- function(df_in){
  df_in$'DCW > ACW'= 0
  n1 = nrow(df_in)
  for(i in 1:n1){
    x1 = as.numeric(df_in$DCW[i])
    x2 = as.numeric(df_in$`Article Chargeable Weight`[i])
    df_in$'DCW > ACW'[i] = Compare_Weight(x1,x2)
  }
  return(df_in)
}

Compute_weight <- function(x2,x1){
  max_num = max(x1,x2)
  if(max_num <= 0.5){
    max_num = ceiling(max_num) - 0.5
  } else{
    max_num = ceiling(max_num)
  }
  return(max_num)
}

Compute_Cubic <- function(n1){
  ans = n1*1000*0.25
  return(ans)
}


Standardise_Header <- function(df_in){
  df_in$`Dest Zone` = 0
  df_in$Postcode = df_in$`Receiver Postcode`
  df_in$`Receiver Postcode`
  df_in <- df_in[, !(names(df_in) %in% "Receiver Postcode")]
  return(df_in)
}

#Classified with Parent Item
Class_Cost_LWs <- function(df_in,df_cost){
  n1 = nrow(df_in)
  n2 = nrow(df_cost)
  
  df_out = data.frame(matrix(nrow = n1*10,ncol = ncol(df_in)))
  colnames(df_out) = colnames(df_in)
  num_row = 1
  
  for(i in 1:n1){
    temp_data = filter(df_cost, Parent.SKU == df_in$OrderItemSKU[i])
    row_temp = nrow(temp_data)
    next_row = row_temp+num_row-1
    if(row_temp == 0){
      df_out[num_row,] = df_in[i,]
      num_row = num_row + 1
    } else {
      df_out[num_row:next_row,] = df_in[i,]
      for(j in 1:row_temp){
        qty = df_in$OrderItemQuantity[i]
        qty_packet = temp_data$Quantity[j]
        price = temp_data$PurchasePrice[j]
        df_out$PurchasePrice[num_row] = price
        df_out$OrderItemQuantity[num_row] = qty*qty_packet
        df_out$OrderItemSKU[num_row] = temp_data$Child.SKU[j]
        df_out$OrderItemTitle[num_row] = temp_data$Child.Title[j]
        num_row = num_row + 1
      }
    }
  }
  df_out <- df_out[!is.na(df_out[,1]),]
  df_out$TP_Line = 0
  return(df_out)
  
}


LW_Rec_TPLine <- function(df_in, index) {
  n <- nrow(df_in)
  for (index in 1:n) {
    df_in$TP_Line[index] <- df_in$PurchasePrice[index] * df_in$OrderItemQuantity[index]
  }
  return(df_in)
}
################################################################################





#Reading Auspost Data



df_AusPost <- read.csv(file = "./input/Aus_Post_Data.csv",header = FALSE)
df_Steadfast <- read.csv(file = "./input/Steadfast_input.csv", header = FALSE)
df_header <- read.csv(file = "./input/Header.csv")
df_MS <- read.csv(file = "./input/Marchship.csv", header = TRUE)
AP_Zone <- read.csv(file = "./input/zone_done.csv", header = TRUE)
LW <- read.csv(file = "./input/LW_input.csv", header = TRUE)
Inv_Data <- read.csv(file = "./input/Inv_Data.csv",header = TRUE)


#Preparing AusPost Information
AusPost.Clean = Create_New(df_AusPost,df_header$AusPost)
AusPost.Clean = Class_Zone(AusPost.Clean,AP_Zone)
AusPost.Clean = Class_AP_Service(AusPost.Clean,df_MS)
AusPost.Clean = Clean_Weight(AusPost.Clean)

#Write AusPost
write.csv(AusPost.Clean, "./output/AustPost Shipping.csv",row.names = FALSE)


#Preparing SteadFast Invoice
df_Steadfast <- df_Steadfast[!is.na(df_Steadfast[,54]),]
SF.Clean = Create_New(df_Steadfast,df_header$Steadfast)
SF.Clean = Standardise_Header(SF.Clean)
SF.Clean = Class_Zone(SF.Clean,AP_Zone)

#Write SteadFast
write.csv(SF.Clean, "./output/Steadfast Shipping.csv",row.names = FALSE)

#Linnwork

LW  = filter(LW, Processed == "True")
LW.Clean = Class_Cost_LWs(LW,Inv_Data)
LW.Clean = LW_Rec_TPLine(LW.Clean,1)






