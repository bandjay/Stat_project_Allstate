##########  Tree models  ################
## try different tree models RF,CART.extreme random tree and XGBoost
## check variable importance from the model.
## try different splitting criterion possible MSE or logRMSE

library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
train <- read_csv("C:/Users/Jay/Desktop/542 project/train.csv/train.csv")
test <- read_csv("C:/Users/Jay/Desktop/542 project/test.csv/test.csv")
dim(train)
dim(test)

## remove these cat vars bcs of difference in levels across test,train
train$cat89=NULL
test$cat89=NULL
train$cat92=NULL
test$cat92=NULL
train$cat96=NULL
test$cat96=NULL
train$cat99=NULL
test$cat99=NULL
train$cat103=NULL
test$cat103=NULL
train$cat106=NULL
test$cat106=NULL
train$cat109=NULL
test$cat109=NULL
train$cat110=NULL
test$cat110=NULL
train$cat111=NULL
test$cat111=NULL
train$cat113=NULL
test$cat113=NULL
train$cat116=NULL
test$cat116=NULL
## XG boost model

library(xgboost)

MAE  <-   function (data,lev = NULL,model = NULL) { 
  out <- c(defaultSummary(data, lev = NULL, model = NULL))
  MAE <- mean(abs(data$obs - data$pred))
  c(out,MAE=MAE)
}
cvControl <- trainControl(method = "cv", number = 5, verbose = TRUE,summaryFunction = MAE)
xgbGrid <-  expand.grid  (nrounds=c(1000), 
                           max_depth=c(20), 
                           eta=c(0.01),
                           gamma= c(1.5),
                           colsample_bytree=c(0.8),
                           min_child_weight=c(1))
 
 xgb_model <- train (loss~.-id,
                         data=train,
                         method = "xgbTree",
                         trControl = cvControl,
                         verbose = TRUE,
                         objective= "reg:linear",
                         metric= "MAE",
                         maximize=FALSE,
                         tuneGrid = xgbGrid)
 test_pred=predict(xgb_model,newdata=test)
 length(test_pred)
 length(test$id)
 sub=data.frame(test$id,test_pred)
 names(sub)=c("id","loss")
 write.csv(sub,"allxgb.csv",row.names = FALSE)
