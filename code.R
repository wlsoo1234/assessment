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
plot(Logistic_Model)

#ROC curve
library(pROC)
lr.predict <- predict(Logistic_Model,train_data, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")

#Fitting a decision tree model
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)




# 7. Artificial Neural Network
install.packages("neuralnet")
library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)

predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)




#8. Gradient Boosting (GBM)
install.packages("gbm")
library(gbm, quietly=TRUE)

# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")
