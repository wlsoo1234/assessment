library(ranger)
library(caret)
library(data.table)
creditcard_data <- read.csv("C:/Users/soowe/Documents/creditcard.csv")

creditcard_data$Amount=scale(creditcard_data$Amount)
NewData=creditcard_data[,-c(1)]
head(NewData)

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
plot(Logistic_Model)

#ROC curve
library(pROC)
lr.predict <- predict(Logistic_Model,train_data2, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")


length(test_data$Class);length(lr.predict)
train_data2 = train_data[0:56961,]

train_data2

#Fitting a decision tree model
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)




