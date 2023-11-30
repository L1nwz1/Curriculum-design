# (1) 使用read.csv()函数读取数据
wine_data <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")

# (2) 将红葡萄酒品质分为两个等级
wine_data$quality_level <- ifelse(wine_data$quality %in% c(3, 4, 5), "bad", "good")
wine_data <- wine_data[,-12]
# (3) 预测变量归一化处理
normalized_data <- as.data.frame(scale(wine_data[, 1:11]))

# (4) 设置随机种子并划分训练集和测试集
set.seed(2333)
train_indices <- sample(1:nrow(normalized_data), 0.7 * nrow(normalized_data))
train_data <- normalized_data[train_indices, ] 
test_data <- normalized_data[-train_indices, ]
train_data$quality_level <- wine_data$quality_level[train_indices]
test_data$quality_level <- wine_data$quality_level[-train_indices]
# (5) 使用nnet程序包的nnet()函数构建神经网络模型
library(nnet)
# 将quality_level列中的bad和good转换为0和1
train_data$quality_level <- ifelse(train_data$quality_level == "bad", 0, 1)
test_data$quality_level <- ifelse(test_data$quality_level == "bad", 0, 1)

model <- nnet(quality_level ~ ., data=train_data, size=2,MaxNWts=7000)

# (6) 在测试集上计算模型的测试错误率
predicted_labels <- predict(model, newdata=test_data)
predicted_nnet <- ifelse(predicted_labels>=0.5, 1, 0)
error_rate <- mean(predicted_nnet != test_data$quality_level)
cat("测试错误率:", error_rate, "\n")

# (7) 选取不同的隐层神经元个数构造神经网络模型，选择测试错误率最低的模型
error_rates <- c()
for (size in 1:10) {
  model <- nnet(quality_level ~ ., data=train_data, size=size)
  predicted_labels <- predict(model, newdata=test_data)
  predicted_nnet <- ifelse(predicted_labels>=0.5, 1, 0)
  error_rate <- mean(predicted_nnet != test_data$quality_level)
  error_rates <- c(error_rates, error_rate)
}
error_rates
best_model_size <- which.min(error_rates)
cat("最佳模型的隐层神经元个数:", best_model_size, "\n")
