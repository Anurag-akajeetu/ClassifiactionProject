library(gbm)
library(plyr)
library(rattle)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(tree)
library(adabag)
library(e1071)
library(resample)
library(lattice)
library(ggplot2)
library(caret)
library(kernlab) 
library(pROC)
library(sandwich)
library(randomForest)
library(nnet)


loadData <- function(){
   train_data = read.csv("C:/Users/aagraw25/Desktop/researchData.csv")
   train_data$X20 <- as.factor(train_data$X3)
   
   train_data_omit <- na.omit(train_data)
   head(test_data)
}

cv_control <- function(){
   ctrl <- trainControl(method="repeatedcv", repeats=5,number = 10)
   levels(train_data$X20)
}

svm_model <- function(){  
  svmfitmodel <- train( train_data$X3 ~., 
                        data = train_data,
                        preProc = c("center","scale"), tuneGrid = expand.grid(.sigma = 0.01, .C=1),
                        method = "svmRadial", trControl=ctrl)
  
  train_data_omit <- na.omit(train_data)
  print(svmfitmodel)
  return(svmfitmodel)
}
svm_radialModel <- function(){
  
  fit <- svm(factor(train_data_omit$X3) ~.,data = train_data_omit, type = "nu-regression", kernel = "radial", cross = 20)
  print(fit)
  print("Predicted using SVM fit model using SVM linear")
  return(fit)
}
  
nnet_model <- function(){
  
  nnetfitmodel <- train( train_data$X3 ~., 
                         data = train_data,
                         preProc = c("center","scale"), 
                         method =  "nnet",
                         trControl=ctrl)
  summary(nnetfitmodel)
  print(nnetfitmodel)
  return(nnetfitmodel)
}
  
prediction <- function(model){
  
  test_data = read.csv("C:/Users/aagraw25/Desktop/testData.csv")
  head(train_data)
  train_data
  result <- predict(model, test_data) 
  return(result)
}
  
main_function <- function(){
  nnetfit_model <- nnet_model()
  svm_radial_Model <- svm_radialModel() 
  svmmodel <- svm_model() 
  result <- prediction(svmmodel)
  print(result)
  
}  
  
  
  
 