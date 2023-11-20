# 导入所需的包
library(glmnet)
library(ncvreg)
library(ISLR)
# 加载Boston数据集
data(Smarket)

# 将数据集分为自变量和因变量
X <- as.matrix(Smarket[, -9])  # 自变量
y_string <- Smarket$Direction  
y <- ifelse(y_string == "Up", 1, -1)
# LASSO
lasso_model <- cv.glmnet(X, y, family="gaussian")
lasso_coef <- coef(lasso_model)
lasso_coef
resid1 <- (X %*% lasso_coef [ -1 ] + lasso_coef [ 1 ] - y)
MSE1 <- sum (resid1 ^ 2)
MSE1
# MCP
fit2 <- cv.ncvreg(X, y, family="gaussian")
fit.mcp <- fit2$fit
beta.fit2 <- fit.mcp$beta [ , fit2$min]
round( beta.fit2 , 3)
resid2 <- (X %*% beta.fit2 [ -1 ] + beta.fit2 [ 1 ] - y)
MSE2 <- sum (resid2 ^ 2)
MSE2
# SCAD
fit3 <- cv.ncvreg(X, y, family="gaussian",penalty="SCAD")
fit.scad <- fit3$fit
beta.fit3 <- fit.scad$beta[, fit3$min]
round(beta.fit3, 3)
resid3 <- (X %*% beta.fit3 [ -1 ] + beta.fit3 [ 1 ] - y)
MSE3 <- sum (resid3 ^ 2)
MSE3

# (2) 比较LASSO方法和逐步回归方法筛选出来的结果
# LASSO
lasso_model <- glmnet(X, y, alpha = 1)
lasso_coef <- coef(lasso_model, s = lasso_model$lambda.min)

# 逐步回归
stepwise_model <- step(lm(y ~ ., data = as.data.frame(X)), direction = "both")
stepwise_model
MSE4 <- sum(stepwise_model$residuals^2)
MSE4
# 打印结果
lasso_coef
coef(stepwise_model)
