
# ch8 变量选择与正则化

# 8.6.1 子集选择法
library ( ISLR )
data ( College )
names ( College )
dim ( College )
# 最优子集法
library ( leaps )
subset.full <- regsubsets ( Apps ~ . , College )
summary ( subset.full )
subset.full <- regsubsets ( Apps ~ . , College , nvmax = 17 )
full.summary <- summary ( subset.full )
names ( full.summary )
full.summary $ rsq
par ( mfrow = c ( 1 , 3 ) )
# CP
which.min ( full.summary $ cp )
plot ( full.summary $ cp , xlab = "Number of Variables" , ylab = "CP" , type = "b" )
points ( 12 , full.summary $ cp [ 12 ] , col = "red" , cex = 2 , pch = 20 )
# BIC
which.min ( full.summary $ bic )
plot ( full.summary $ bic , xlab = "Number of Variables" , ylab = "BIC" , type = "b" )
points ( 10 , full.summary $ bic [ 10 ] , col = "red" , cex = 2 , pch = 20 )
# Adjust Rsq
which.max ( full.summary $ adjr2 )
plot ( full.summary $ adjr2 , xlab = "Number of Variables" , ylab = "Adjusted RSq" , type = "b" )
points ( 13 , full.summary $ adjr2 [ 13 ] , col = "red" , cex = 2 , pch = 20 )
coef ( subset.full , 10 )
# 向前逐步选择法
subset.fwd <- regsubsets ( Apps ~ . , College , nvmax = 17 , method = "forward" )
summary ( subset.fwd )
# 向后逐步选择法
subset.bwd <- regsubsets ( Apps ~ . , College , nvmax = 17 , method = "backward" )
summary ( subset.bwd )
coef ( subset.full , 8 )
coef ( subset.fwd , 8 )
coef ( subset.bwd , 8 )

# 8.6.2 单变量选择
# （1）线性回归
x <- matrix ( rnorm ( 100*20 ) , 100 , 20 )
beta <- c ( seq ( 1 , 2 , length.out = 6 ) , 0 , 0 , 0 ,  0 , rep ( 1 , 10 ) )
y <- x %*% beta + rnorm ( 100 )
# LASSO惩罚
library ( glmnet )
fit1 <- cv.glmnet ( x , y , family = "gaussian" )
beta.fit1 <- coef ( fit1 ) # 提取参数的估计值
beta.fit1
resid1 <- ( x %*% beta.fit1 [ -1 ] + beta.fit1 [ 1 ] - y )
MSE1 <- sum ( resid1 ^ 2 ) # 计算残差平方和
MSE1
# MCP惩罚
library ( ncvreg )
fit2 <- cv.ncvreg ( x , y , family = "gaussian" )
fit.mcp <- fit2$fit
beta.fit2 <- fit.mcp$beta [ , fit2$min ] # 提取参数的估计值
round ( beta.fit2 , 3 ) # 保留三位小数
resid2 <- ( x %*% beta.fit2 [ -1 ] + beta.fit2 [ 1 ] - y )
MSE2 <- sum( resid2 ^ 2 ) # 计算残差平方和
MSE2
# SCAD惩罚
fit3 <- cv.ncvreg ( x , y , family = "gaussian" , penalty = "SCAD" )
fit.scad <- fit3$fit
beta.fit3 <- fit.scad$beta [ , fit3$min ] # 提取参数的估计值
round ( beta.fit3 , 3 ) # 保留三位小数
resid3 <- ( x %*% beta.fit3 [ -1 ] + beta.fit3 [ 1 ] - y )
MSE3 <- sum( resid3 ^ 2 ) # 计算残差平方和
MSE3
# （2）Logistic回归
library ( ncvreg )
data ( heart )
x <- as.matrix ( heart [ , 1 : 9 ] )
y <- heart $ chd
# LASSO惩罚
library ( glmnet )
fit1 <- cv.glmnet ( x , y , family = "binomial" )
beta.fit1 <- coef ( fit1 )
beta.fit1
# MCP惩罚
fit2 <- cv.ncvreg ( x , y , family = "binomial" )
fit.mcp <- cvfit.mcp $ fit
beta.fit2 <- fit.mcp $ beta [ , fit2 $ min]
beta.fit2
# SCAD惩罚
fit3 <- cv.ncvreg ( x , y , family = "binomial" , penalty = "SCAD" )
fit3 <- cvfit.scad $ fit
beta.fit3 <- fit.scad $ beta [ , fit3 $ min]
beta.fit3

# 8.6.3 组变量选择
# （1）线性回归
# Group LASSO
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1 : -2 ] )
y <- birthwt.grpreg $ bwt
colnames ( X )
group <- c ( 1 , 1 , 1 , 2 , 2 , 2 , 3 , 3 , 4 , 5 , 5 , 6 , 7 , 8 , 8 , 8 )  # 变量组结构
cvfit <- cv.grpreg ( X , y , group , penalty = "grLasso" )
coef ( cvfit ) # Beta at minimum Cross-Validation Error
# 弹性网
library ( glmnet )
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1:-2 ] )
y <- birthwt.grpreg $ bwt
fit.enet <- cv.glmnet ( X , y , family = "gaussian" , alpha = 0.2 )
beta.enet <- coef ( fit.enet )
beta.enet
# （2）Logistic回归
# Group LASSO
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1 : -2 ] )
y <- birthwt.grpreg$low
group <- c ( 1 , 1 , 1 , 2 , 2 , 2 , 3 , 3 , 4 , 5 , 5 , 6 , 7 , 8 , 8 , 8 ) # 变量的分组结构
cvfit <- cv.grpreg ( X , y , group , penalty = "grLasso" )
coef ( cvfit ) # Beta at minimum Cross-Validation Error
summary ( cvfit )
plot ( cvfit )
# 弹性网
y <- birthwt.grpreg $ low
fit.enet <- cv.glmnet ( X , y , family = "binomial" , alpha = 0.2 )
beta.enet <- coef ( fit.enet )
beta.enet

# 8.6.4 双层变量选择
# （1）线性回归
# Group Bridge
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1 : -2 ] )
y <- birthwt.grpreg $ bwt
group <- c ( 1 , 1 , 1 , 2 , 2 , 2 , 3 , 3 , 4 , 5 , 5 , 6 , 7 , 8 , 8 , 8 ) # 变量的分组结构
cvfit.b <- gBridge ( X , y , group ) # L1 group bridge
select ( cvfit.b ) $ beta
# Composite MCP
cvfit.m <- cv.grpreg ( X , y , group , penalty = "cMCP" , gama = 2.5 ) 
coef ( cvfit.m )
# SGL
library ( SGL )
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1 : -2 ] )
y <- birthwt.grpreg $ bwt
group <- c ( 1 , 1 , 1 , 2 , 2 , 2 , 3 , 3 , 4 , 5 , 5 , 6 , 7 , 8, 8 ,8) # 变量的分组结构
data <- list ( x = X , y = y )
cvFit <- cvSGL ( data , group , type = "linear" ) # SGL
lambda.min <- which.min ( cvFit $ lldiff )
cvFit $ fit $ beta [ , lambda.min ]
# （2）Logistic回归
# Group Bridge
library ( grpreg )
cvfit.b <- gBridge ( X , y , group , family = "binomial" ) 
select ( cvfit.b ) $ beta
# Composite MCP
cvfit.m <- cv.grpreg ( X , y , group , penalty = "cMCP" , family = "binomial" )
coef ( cvfit.m )
# SGL
library ( SGL )
library ( grpreg )
data ( birthwt.grpreg )
X <- as.matrix ( birthwt.grpreg [ , -1 : -2 ] )
y <- birthwt.grpreg$low
group <- c ( 1 , 1 , 1 , 2 , 2 , 2 , 3 , 3 , 4 , 5 , 5 , 6 , 7 , 8 , 8 , 8 ) # 变量的分组结构
data <- list ( x = X , y = y )
cvFit <- cvSGL ( data , group , type = "logit" )
lambda.min <- which.min ( cvFit $ lldiff ) # 最优 值
cvFit $ fit $ beta [ , lambda.min ] # 最优 值时回归系数的估计结果
cvFit $ fit $ intercepts [ lambda.min ]

# 8.6.5 案例分析
# （1）惩罚线性回归下医疗支出分析
# 描述性统计分析
library ( corrgram )
corrgram ( prostate )
# 建立模型
library ( "ElemStatLearn" )
data ( prostate )
y <- prostate $ lpsa
x <- as.matrix ( prostate [ , 1 : 8 ] )
n <- length ( y )
index <- sample ( n , 0.7 * n , replace = F ) # 随机抽取训练集样本的序号
x.train <- x [ index , ] # 训练集
y.train <- y [ index ]
x.test <- x [ -index , ] # 测试集
y.test <- y [ -index ]
# MCP
library ( ncvreg )
fit1<- cv.ncvreg ( x.train , y.train , family = "gaussian" )
fit.mcp <- fit1 $ fit
beta.fit1 <- fit.mcp$beta [ , fit1 $ min ]
resid <- ( x.test %*% beta.fit1 [ -1 ] + beta.fit1 [ 1 ] - y.test )
MSE1 <- sum ( resid ^ 2 )
# group lasso 
library ( grpreg )
group <- c ( 1 , 2 , 3 , 4 , 1 , 5 , 6 , 6 ) # 变量的分组结构
fit2 <- cv.grpreg ( x.train , y.train , group , penalty = "gLasso" )
beta.fit2 <- coef ( fit2 ) # Beta at minimum Cross-Validation Error
resid <- ( x.test %*% beta.fit2 [ - 1 ] + beta.fit2 [ 1 ] - y.test )
MSE2 <- sum ( resid ^ 2 )
# Composite MCP
group <- c ( 1 , 2 , 3 , 4 , 1 , 5 , 6 , 6 ) 
fit3 <- cv.grpreg ( x.train , y.train , group , penalty = "cMCP" , 
                    family = "gaussian" , gama = 2.5 ) 
beta.fit3 <- coef ( fit3 )
resid <- ( x.test %*% beta.fit3 [ - 1 ] + beta.fit3 [ 1 ] - y.test )
MSE3 <- sum ( resid ^ 2 )
MSE3
# 估计结果
round ( beta.fit1 , 3 )
MSE1
round ( beta.fit2 , 3 )
MSE2
round ( beta.fit3 , 3 )
MSE3
# （2）Logistic回归的信用违约风险因素分析
n <- length( y )
p <- ncol ( x )
group <- c ( 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 9 , 9 , 10 , 11 , 
             12 , 12 , 12 , 12 , 13 : 18 , 19 , 19 , 19 , 19 , 19 )
library ( SGL )
library ( glmnet )
library ( grpreg )
# LASSO
fit1 <-cv.glmnet ( as.matrix ( x ) , y , family = "binomial" )
beta.lasso <- coef ( fit1 )
# group lasso
fit2 <- cv.grpreg ( x , y , group , penalty = "gLasso" )
beta.glasso <- coef ( fit2 )
# Composite MCP
fit3<- cv.grpreg ( x , y , group , penalty = "cMCP" , family = "binomial" , gama = 3 ) 
beta.cmcp <- coef ( fit3 )
beta.lasso
round ( beta.glasso , 4 )
round ( beta.cmcp , 4 )
