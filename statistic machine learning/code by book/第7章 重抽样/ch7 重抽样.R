
# ch 7 R代码

# 7.5.1 验证集方法
library ( MASS )
data ( Boston )
dim ( Boston )
set.seed ( 1 )
train1 <- sample ( 506 , 506 / 2 )
lmfit1 <- lm ( medv ~. , data = Boston , subset = train1 )
attach ( Boston )
pred1 <- predict ( lmfit1 , Boston [ - train1 , ] )
mean ( ( medv [ - train1 ] - pred1 ) ^ 2 )
err1 <- rep ( 0 , 10 )
for ( i in 1 : 10 ) {
        train2 <- sample ( 506 , 506 / 2 )
        lmfit2 <- lm ( medv ~. , data = Boston , subset = train2 )
        pred2 <- predict ( lmfit2 , Boston [ - train2 , ] )
        err1 [ i ] <- mean ( ( medv [ - train2 ] - pred2 ) ^ 2 )
}
plot ( 1 : 10 , err1 , xlab = "" , ylim = c ( 20 , 30 ) , type = "l" , 
       main = "选取10个不同的训练集对应的测试误差" )
detach ( Boston )

# 7.5.2 留一交叉验证法
library ( boot )
glmfit1 <- glm ( medv ~. , data = Boston )
cv.err1 <- cv.glm ( Boston , glmfit1 )
cv.err1 $ delta

# 7.5.3 K折交叉验证法
set.seed ( 3 )
glmfit2 <- glm ( medv ~. , data = Boston )
cv.err2 <- cv.glm ( Boston , glmfit2 , K = 10 )
cv.err2 $ delta
err2 <- rep ( 0 , 10 )
for ( i in 1 : 10 ) {
        glmfit3 <- glm ( medv ~. , data = Boston )
        cv.err3 <- cv.glm ( Boston , glmfit3 , K = 10 )
        err2 [ i ] <- cv.err3 $ delta [ 1 ]
}
plot ( 1 : 10 , err2 , xlab = "" , ylim = c ( 20 , 30 ) , type = "l" , 
       main = "10次不同的CV误差" )

# 7.5.4 自助法
boot.f <- function ( data , index ){
        fit <- lm ( medv ~ lstat , data = data , subset = index )
        return ( coef ( fit ) )
}
boot.f ( Boston , 1 : 506 )
set.seed ( 4 )
boot.f ( Boston , sample ( 506 , 506 , replace = T ) )
boot ( Boston , boot.f , 1000 )
fit <- lm ( medv ~ lstat , data = Boston )
summary ( fit ) $ coef

