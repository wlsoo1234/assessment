install.packages("ranger")
install.packages("caret")
install.packages("data.table")
library(ranger)
library(caret)
library(data.table)
creditcard_data <- read.csv("C:/Users/soowe/Documents/creditcard.csv")

dim(creditcard_data)
head(creditcard_data,6)

table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)

sd(creditcard_data$Amount)