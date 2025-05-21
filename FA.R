library(lavaan)
library(psych)
library(factoextra)
setwd("C:\\Users\\huann\\OneDrive\\Desktop\\Education Big Data\\原始資料")
data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)
sum(is.na(data1))
data1_complete <- na.omit(data1)
data_items <- data1_complete[ ,grep("^v[0-9]+$" ,names(data1_complete))]
eigenvalues <- eigen(cor(data_items))$values
print(eigenvalues)

efa_result <- fa(r = data_items, nfactors = 6, rotate = "varimax")
print(efa_result)
