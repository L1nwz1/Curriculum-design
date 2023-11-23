# 导入ISLR包
library(ISLR)

# 设置随机种子以确保可重复性
set.seed(233)

# 加载所需的包
library(tree)
library(randomForest)
library(gbm)
library(xgboost)
library(pROC)

# 加载Default数据集
data(Default)

# 划分训练集和测试集
train_index <- sample(1:nrow(Default), 0.6 * nrow(Default))
train_data <- Default[train_index, ]
test_data <- Default[-train_index, ]

# 决策树模型
tree_model <- tree(default ~ ., data = train_data)
tree_train_pred <- predict(tree_model, train_data,type="class")
tree_test_pred <- predict(tree_model, test_data,type="class")

table(tree_train_pred, train_data$default)
table(tree_test_pred, test_data$default)

# 随机森林模型
rf_model <- randomForest(default ~ ., data = train_data, importance=TRUE)
rf_train_pred <- predict(rf_model, train_data, type="class")
rf_test_pred <- predict(rf_model, test_data, type="class")

table(rf_train_pred, train_data$default)
table(rf_test_pred, test_data$default)

# Adaboost模型

# 将非数值项转换为0或1
train.num <- train_data
test.num <- test_data
train.num$default<-as.numeric(train_data$default) - 1
train.num$student<-as.numeric(train_data$student) - 1
test.num$default<-as.numeric(test_data$default) - 1
test.num$student<-as.numeric(test_data$student) - 1

adaboost_model <- gbm(default ~ ., data = train.num, distribution = "adaboost",n.trees = 5000,interaction.depth = 4)
summary(adaboost_model)
adaboost_train_pred <- predict(adaboost_model, train.num, n.trees = 5000,type="response")
adaboost_train_pred <- ifelse(adaboost_train_pred > 0.5, 1, 0)
adaboost_test_pred <- predict(adaboost_model, test.num, n.trees = 5000,type="response")
adaboost_test_pred <- ifelse(adaboost_test_pred > 0.5, 1, 0)
table(adaboost_train_pred, train.num$default)
table(adaboost_test_pred, test.num$default)

# XGBoost模型
xgtrain_s <-Matrix::sparse.model.matrix(default~.-1, data = train_data)
xgtest_s <-Matrix::sparse.model.matrix(default~.-1, data = test_data)
dtrain <- xgb.DMatrix(data = xgtrain_s,label = train_data$default)
dtest <- xgb.DMatrix(data = xgtest_s,label = test_data$default)

xgboost_model <- xgboost(data = dtrain, max.depth=10,min_child_weight=1,gamma=0.1,colsample_bytree=0.8,subsample=0.8,scale_pos_weight=1,eta=0.1,eval_metric="auc",nround=10000,silent=TRUE)
xgboost_train_pred <- predict(xgboost_model, dtrain)
xgboost_test_pred <- predict(xgboost_model, dtest)


table(as.numeric(xgboost_train_pred > 0.5), train_data$default) 
table(as.numeric(xgboost_test_pred > 0.5), test_data$default) 
# Logistic回归模型
logistic_model <- glm(default ~ ., data = train_data, family = "binomial")
logistic_train_pred <- predict(logistic_model, train_data, type = "response")
logistic_test_pred <- predict(logistic_model, test_data, type = "response")
logistic_train_pred <- ifelse(logistic_train_pred > 0.5, 1, 0)
logistic_test_pred <- ifelse(logistic_test_pred > 0.5, 1, 0)
table(logistic_train_pred, train_data$default)
table(logistic_test_pred, test_data$default)
# 计算训练集和测试集的预测准确率
train_accuracy <- function(pred, actual) {
  mean(pred == actual)
}

test_accuracy <- function(pred, actual) {
  mean(pred == actual)
}

# 计算各模型的训练集和测试集预测准确率
# 计算各模型的ROC曲线和AUC值
train_accuracy <- function(pred, actual) {
  mean(pred == actual)
}

test_accuracy <- function(pred, actual) {
  mean(pred == actual)
}
train.num$default
models <- c("决策树", "随机森林", "Adaboost", "XGBoost", "Logistic回归")
train_acc <- c(train_accuracy(tree_train_pred, train_data$default),
               train_accuracy(rf_train_pred, train_data$default),
               train_accuracy(adaboost_train_pred, train.num$default),
               train_accuracy(as.numeric(xgboost_train_pred > 0.5), train.num$default),
               train_accuracy(logistic_train_pred, train.num$default))
               

test_acc <- c(test_accuracy(tree_test_pred, test_data$default),
              test_accuracy(rf_test_pred, test_data$default),
              test_accuracy(adaboost_test_pred, test.num$default),
              test_accuracy(as.numeric(xgboost_test_pred > 0.5), test.num$default),
              test_accuracy(logistic_test_pred, test.num$default))

result <- data.frame(Model = models, Train_Accuracy = train_acc, Test_Accuracy = test_acc)
result
# auc
roc_curve <- roc(test_data$default, as.numeric(tree_test_pred))
auc_tree <- auc(roc_curve)

roc_curve <- roc(test_data$default, as.numeric(rf_test_pred))
auc_rf <- auc(roc_curve)

roc_curve <- roc(test_data$default, as.numeric(adaboost_test_pred))
auc_adaboost <- auc(roc_curve)

roc_curve <- roc(test_data$default, as.numeric(xgboost_test_pred))
auc_xgboost <- auc(roc_curve)

roc_curve <- roc(test_data$default, as.numeric(logistic_test_pred))
auc_logistic <- auc(roc_curve)
# 打印结果
auc_result <- data.frame(Model = models, AUC = c(auc_tree, auc_rf, auc_adaboost, auc_xgboost, auc_logistic))
auc_result
