
# ch5 R代码

# 5.4.1 一元线性回归
consum <- c ( 594 , 638 , 1122 , 1155 , 1408 , 1595 , 1969 , 2078 , 2585 , 2530 )
income <- c ( 800 , 1100 , 1400 , 1700 , 2000 , 2300 , 2600 , 2900 , 3200 , 3500 )
lm1 <- lm ( consum ~ income ) # 将回归结果保存在lm1对象里
coef ( lm1 ) # 提取估计系数
coef ( lm ( consum ~ - 1 + income ) )
lm2 <- lm ( y ~ x )
coef ( lm2 )
slm <- summary ( lm1 )
slm $ sigma # 得到总体方差的OLS估计量
slm $ coef # 得到系数有关的矩阵
slm $ coef [ , 2 ] # 矩阵第二列，即系数标准差
slm2 <- summary ( lm2 )
slm2 $ sigma
slm2 $ coef
slm2 $ coef [ , 2 ]
slm $ r.squared
slm2 $ r.squared
slm <- summary ( lm1 )
slm
slm $ coef [ , 3 ] # 提取t值
slm $ coef [ , 4 ] # 提取t值的p-value
predict ( lm1 , newdata = data.frame ( income = 4000 ) , interval = "confidence" , level = 0.95 )
predict ( lm1 , newdata = data.frame ( income = 4000 ) ,  interval = "prediction" , level = 0.95 )
sx <- sort ( income ) # 把自变量先从小到大排序
# 求均值的预测区间
conf <- predict ( lm1 , data.frame ( income = sx ) , interval = "confidence" ) 
# 求个值的预测区间
pred <- predict ( lm1 , data.frame ( income = sx ) , interval = "prediction" ) 
plot ( income , consum ) # 画散点图
abline ( lm1 ) # 添加回归线
lines ( sx , conf [ , 2 ] ) ; lines ( sx , conf [ , 3 ] ) 
lines ( sx , pred [ , 2 ] , lty = 3 ) ; lines ( sx , pred [ , 3] , lty = 3 )

# 5.4.2 多元线性回归
dat <- read.csv ( file = "tax.csv" ) # 读入csv格式的数据
lm3 <- lm ( tax ~ GDP + expand + CPI , data = dat )
coef ( lm3 )
summary ( lm3 )
slm3 <- summary ( lm3 )
slm3 $ r.squared
slm3 $ adj.r.squared
summary ( lm3 ) $ coef
coef ( lm3 )
coef ( lm3 ) [ 1 ] + coef ( lm3 ) [ 2 ] * 520000 + coef ( lm3 ) [ 3 ] * 130000 + coef (lm3 ) [ 4 ] * 103
predict ( lm3 , newdata = data.frame ( GDP = 520000 , expand = 130000 , CPI = 103 ) )
slm $ coef [ , 1 ] [ 1 ] + slm $ coef [ , 1 ] [ 2 ] * 4000
coef ( lm1 ) [ 1 ] + coef ( lm1 ) [ 2 ] * 4000
predict ( lm1 , newdata = data.frame ( income = 4000 ) )
fitted ( lm1 )
resid ( lm1 )

