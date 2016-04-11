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


clear_all <- function(){
  rm (list = ls())
}

load_data <- function(){
  train_data = read.csv("C:/Users/aagraw25/Desktop/DataMining/NewTrain_data.csv")
  train_data$X20 <- as.factor(train_data$X20)
  head(train_data)
}

cv_control <- function(){
  ctrl <- trainControl(method="repeatedcv", repeats=5,number = 10)
  levels(train_data$X20)
}

svm_model <- function(){
 
  svmfitmodel <- train( train_data$X20 ~., 
                      data = train_data,
                      preProc = c("center","scale"), tuneGrid = expand.grid(.C=1),
                      method = "svmLinear", trControl=ctrl)
  
  print(svmfitmodel)
  
  print("Predicted using SVM fit model using SVM linear")
}

svm_model_poly <- function(){
  
  svmfitmodelPoly <- train( train_data$X20 ~., 
                        data = train_data,
                        preProc = c("center","scale"), tuneGrid = expand.grid(.degree = 3,.scale = 0.01, .C = 0.1),
                        method = "svmPoly", trControl=ctrl)
  
  print(svmfitmodelPoly)
  
  print("Predicted using SVM fit model using SVM Poly")
}



svm_model_radial <- function(){
  
  svmfitmodelradial <- train( train_data$X20 ~.,
                            data = train_data,
                            preProc = c("center","scale"), tuneGrid = expand.grid(.sigma = 0.01),
                            method = "lssvmRadial", trControl=ctrl)


  # svmfitmodelradial <- train( train_data$X20 ~., 
  #                             data = train_data,
  #                             preProc = c("center","scale"), tuneGrid = expand.grid(.C = 9),
  #                             method = "svmRadialCost", trControl=ctrl)
  
  # svmfitmodelradial <- train( train_data$X20 ~., 
  #                             data = train_data,
  #                             preProc = c("center","scale"), tuneGrid = expand.grid(.C = 9),
  #                             method = "svmRadialCost", trControl=ctrl)
  # 
  print(svmfitmodelradial)
  
  print("Predicted using SVM fit model using SVM Poly")
  return(svmfitmodelradial)
}



bay_model <- function(){
  
  bayfitmodel <- train( train_data$X20 ~., 
                        data = train_data,
                        preProc = c("center","scale"), 
                        method = "bayesglm", trControl=ctrl)
  
  print(bayfitmodel)
}

lr_model <- function(){
  
  lrfitmodel <- train( train_data$X20 ~., 
                        data = train_data,
                        preProc = c("center","scale"), 
                        method = "logreg" ,
                       trControl=ctrl)
  
  print(lrfitmodel)
}

rf_model <- function(){
  
  rffitmodel <- train( train_data$X20 ~., 
                       data = train_data,
                       preProc = c("center","scale"), 
                       method =  "rf",tuneGrid = expand.grid(.mtry = 2),
                       trControl=ctrl)
  
  print(rffitmodel)
}
gbm_model <- function(){
  
  gbmfitmodel <- train( train_data$X20 ~., 
                       data = train_data,
                       preProc = c("center","scale"), 
                       method =  "gbm", 
                       tuneGrid = expand.grid(.trees = 1000, .depth = 3, .shrinkage = 0.1, .minobsinnode = 20),
                       trControl=ctrl)
  
  print(gbmfitmodel)
}
knn_model <- function(){
  
  knnfitmodel <- train( train_data$X20 ~., 
                        data = train_data,
                        preProc = c("center","scale"), 
                        method =  "knn",
                        trControl=ctrl)
  
  print(knnfitmodel)
}


net_model <- function(){
  
  nnetfitmodel <- train( train_data$X20 ~., 
                        data = train_data,
                        preProc = c("center","scale"), 
                        method =  "nnet",
                        trControl=ctrl)
  
  print(nnetfitmodel)
}

prediction <- function(model){
  
  test_data = read.csv("C:/Users/aagraw25/Desktop/DataMining/test_data.csv")
  head(test_data)
  result <- predict(model, test_data) 
  return(result)
}

main_function <- function(){
  load_data()
  cv_control()
  svm_model()
  svm_model_poly()
  svmmodelradial <- svm_model_radial()
  bay_model()
  #lr_model()
  rf_model()
  gbm_model()
  knn_model()
  ada_model()
  result <- prediction(svmmodelradial)
  result
  print(result)
  write(result, file = "resultData.csv",
        ncolumns = 1,
        append = FALSE, sep = " ")
}

