
library(data.table)
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
df <- rbindlist(temp)
setwd("./output")
write.csv(df, file="Combined.csv",row.names = FALSE)
setwd('..')