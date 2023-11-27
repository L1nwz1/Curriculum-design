# 导入ISLR包
library(ISLR)

# (1) 将mpg按照中位数划分为两类，新增一个变量grade，并用0和1分别表示
Auto$grade <- ifelse(Auto$mpg >= median(Auto$mpg), 1, 0)

# (2) 随机划分训练集和测试集
set.seed(114514)  # 设置随机种子，保证结果可复现
train_indices <- sample(1:nrow(Auto), 292)  # 随机选择292个样本作为训练集
train_data <- Auto[train_indices, ]  # 训练集数据
test_data <- Auto[-train_indices, ]  # 测试集数据

# (3) 使用maximal margin classifier进行建模，并进行交叉验证选择最优模型
library(e1071)  # 导入e1071包，其中包含了svm函数和tune函数

# 定义参数网格
tune_grid <- list(C = c(0.01, 0.1, 0.5, 1, 5, 10, 50, 100))

# 交叉验证选择最优模型
tuned_model <- tune(svm, grade ~ ., data = train_data, kernel = "linear", ranges = tune_grid)

# 输出最优模型的结果
print(tuned_model)

# 使用最优模型对测试集进行预测
predictions <- predict(tuned_model$best.model, newdata = test_data)
# 将预测结果按照>=0.5为1，<0.5为0重新组合成数据
predicted_data <- ifelse(predictions >= 0.5, 1, 0)
table(true = test_data$grade, predict=predicted_data)

# (4) 使用radial kernel的SVM对训练集进行建模，并进行交叉验证选择最优模型

# 定义参数网格
tune_grid <- list(cost = c(0.01, 0.1, 0.5, 1, 5, 10, 50, 100), gamma = c(0, 1, 2, 5, 10))

# 交叉验证选择最优模型
tuned_model <- tune(svm, grade ~ ., data = train_data, kernel = "radial", ranges = tune_grid)

# 输出最优模型的结果
summary(tuned_model)

# 使用最优模型对测试集进行预测
predictions <- predict(tuned_model$best.model, newdata = test_data)
predicted_data <- ifelse(predictions >= 0.5, 1, 0)
table(true = test_data$grade, predict=predicted_data)