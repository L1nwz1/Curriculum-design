# 导入ISLR包
library(ISLR)

# 查看数据基本信息
summary(OJ)

# 设置随机种子以确保可重复性
set.seed(2333)

# 划分训练集和测试集
train_index <- sample(1:nrow(OJ), 0.7 * nrow(OJ))
train_data <- OJ[train_index, ]
test_data <- OJ[-train_index, ]

# 导入tree包
library(tree)

# 建立一棵树
tree_model <- tree(Purchase ~ ., data = train_data)

# 查看树的输出信息
summary(tree_model)

plot(tree_model)
text(tree_model, pretty=0)

# 剪枝前预测训练、测试数据的响应值
train_pred <- predict(tree_model, train_data, type = "class")
test_pred <- predict(tree_model, test_data,type="class")

# 剪枝前计算训练、测试错误率
train_data $ Purchase
train_pred
table(train_pred, train_data $ Purchase)
table(test_pred, test_data $ Purchase)

# 进行树的剪枝
cv_tree <- cv.tree(tree_model,FUN = prune.misclass)
cv_tree
# 画出错误率对size的函数图形
plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "Size", ylab = "Deviance")

# 确定最优的树
prune_tree <- prune.misclass(tree_model, best = 5)
plot(prune_tree)
text(prune_tree, pretty=0)

#剪枝后训练、测试错误率
cv.train.pred <-predict(prune_tree, train_data, type = "class")
cv.test.pred <- predict(prune_tree, test_data, type="class")
table(cv.train.pred, train_data$Purchase)
table(cv.test.pred, test_data$Purchase)
