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

View(creditcard_data)

head(creditcard_data)

creditcard_data$Amount=scale(creditcard_data$Amount)
NewData=creditcard_data[,-c(1)]
head(NewData)
View(NewData)

#Data Modeling 
#After we have standardized our entire dataset, 
#we will split our dataset into training set as well as test 
#set with a split ratio of 0.80. This means that 80% of our data 
#will be attributed to the train_data whereas 20% will be attributed
#to the test data. We will then find the dimensions using the dim()
#function -
install.packages("caTools")
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)


#Fitting Logistic Regression Model

Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
