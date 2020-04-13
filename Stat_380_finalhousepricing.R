rm(list=ls())
library(data.table)
#library(caret)
#library(Metrics)
#library(glmnet)
#library(plotmo)
#library(lubridate)
library(xgboost)
library(caret)
library(rpart)
library(dplyr)
train <- read.csv("Stat_380_train.csv", stringsAsFactors = F)
test <- read.csv("Stat_380_test.csv", stringsAsFactors = F)
numvar <- names(train)[which(sapply(train, is.numeric))]
train <- train[,numvar]
numvar_test<-numvar[-length(numvar)]
test <- test[,numvar_test]

train %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) -> train

test %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) -> test

train %>% mutate(year_old = YrSold - YearBuilt) -> train

test %>% mutate(year_old = YrSold - YearBuilt) -> test

trctrl <- trainControl(method = "cv", number = 5)

tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

xgboost_fit <- train(SalePrice ~., data = train, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)


# Testing
test_predict <- predict(xgboost_fit, test)

final.submit <- cbind(test$Id,test_predict)
write.table(final.submit,file="submission1.csv",sep=",",
            quote=FALSE,col.names=c("Id","SalePrice"),row.names=FALSE)