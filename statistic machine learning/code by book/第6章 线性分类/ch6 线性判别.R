
# ch 6 linear classification

grade <- read.table ( file = "grade.txt" , header = T )
View(grade)

# 6.5.1 descriptive statistics��

summarys <- function ( x ) {
        list ( mean = mean ( x ) , max = max ( x ) , min = min ( x ) , sd = sd ( x ) )
} # 自编一个求基本描述统计量简单函�?          
summarys ( subset ( grade , PSI == 0 ) $ GRADE ) # subset ( )筛选PSI = 0的数�?
summarys ( subset ( grade , PSI == 1 ) $ GRADE )
summarys ( grade $ GRADE )
summarys ( subset ( grade , PSI == 0 ) $ GPA )
summarys ( subset ( grade , PSI == 1 ) $ GPA )
summarys ( grade $ GPA )
summarys ( subset ( grade , PSI == 0 ) $ TUCE )
summarys ( subset ( grade , PSI == 1 ) $ TUCE )
summarys ( grade $ TUCE )
summarys ( subset ( grade , PSI == 0 ) $ PSI )
summarys ( subset ( grade , PSI == 1 ) $ PSI )
summarys ( grade $ PSI )

# 6.5.2 Logistic模型
# OLS估计
lpm <- lm ( GRADE ~ GPA + TUCE + PSI , data = grade )
summary ( lpm )
plot(grade$GPA,grade$GRADE,xlab="GPA",ylab="GRADE",type="n")
points(jitter(grade$GPA),grade$GRADE,cex=1,pch="l",col="green")
lines(grade$GPA,fitted(lm(GRADE~GPA,data=grade)))

# Probit模型估计
grade.probit <- glm ( GRADE ~ GPA + TUCE + PSI , 
                      family = binomial ( link = "probit" ) , data = grade ) # Probit模型
summary ( grade.probit )
install.packages ( "lmtest" )
library ( lmtest ) # 需首先安装lmtest包，并载入包
lrtest ( grade.probit ) # LR检�?
McFa.Rsquare <- function ( glm.object ) { # glm ( )估计结果作为函数的输入变�?
        deviance <- glm.object $ deviance 
        null.deviance <- glm.object $ null.deviance
        McFa.Rsquare <- 1 - ( deviance / null.deviance )
        list ( McFadden.Rsquare = McFa.Rsquare )
}
McFa.Rsquare ( grade.probit )
# Logit模型估计
grade.logit <- glm ( GRADE ~ GPA + TUCE + PSI , 
                     family = binomial (link = "logit" ) , data = grade) # 注意link设为logit
summary ( grade.logit )
lrtest ( grade.logit ) # LR检�?
McFa.Rsquare ( grade.logit )
coe <- coef ( grade.probit ) # 提取probit模型系数
probit <- dnorm ( coe [ 1 ] + coe [ 2 ] * mean ( grade $ GPA ) + 
                          coe [ 3 ] * mean ( grade $ TUCE ) + coe [ 4 ] * mean ( grade $ PSI ) ) 
# 求probit模型平均边际影响
( m.gpa = coe [ 2 ] * probit ) # 求GPA平均边际影响
( m.tuce = coe [ 3 ] * probit )
( m.PSI = coe [ 4 ] * probit )
coe.l <- coef ( grade.logit ) # 提取logit模型系数
logit <- dlogis ( coe.l [ 1 ] + coe.l [ 2 ] * mean ( grade $ GPA ) + 
                          coe.l [ 3 ] * mean ( grade $ TUCE ) + coe.l [ 4 ] * mean ( grade$PSI ) )
# 求logit模型平均边际影响
( m.gpa.l = coe.l [ 2 ] * logit )
( m.tuce.l = coe.l [ 3 ] * logit )
( m.PSI.l = coe.l [ 4 ] * logit )

# 6.5.3 判别分析
# 线性判别分�?
library ( MASS )
lda.fit <- lda ( GRADE ~ GPA + TUCE + PSI , data = grade )
lda.fit
plot ( lda.fit )
lda.pred <- predict ( lda.fit, grade )
names ( lda.pred )
lda.class <- lda.pred $ class
table ( lda.class , grade $ GRADE , dnn = c ( "Prediction" , "Actual" ) )
mean ( lda.class != grade $ GRADE )
# 二次判别分析
qda.fit <- qda ( GRADE ~ GPA + TUCE + PSI , data = grade )
qda.fit
qda.pred <- predict ( qda.fit , grade )
names ( qda.pred )
qda.class <- qda.pred $ class
table ( qda.class , grade $ GRADE , dnn = c ( "Prediction" , "Actual" ) )
mean ( qda.class != grade $ GRADE )
# Naive Bayes判别分析
library ( e1071 )
bayes.fit <- naiveBayes( as.factor ( GRADE ) ~ GPA + TUCE + PSI , data = grade )
bayes.fit
bayes.pred <- predict ( bayes.fit , grade )
table ( bayes.pred , grade $ GRADE , dnn = c ( "Prediction" , "Actual" ) )
mean ( bayes.pred != grade $ GRADE )

# 6.5.4 模型比较
# 错分�?
logit.pred <- predict ( grade.logit , grade , type = "response" )
logit.class <- rep ( 0 , nrow ( grade ) )
logit.class [ logit.pred > 0.5 ] <- 1 #阈值设�?0.5
table ( logit.class , grade $ GRADE , dnn = c ( "Prediction" , "Actual" ) )
mean ( logit.class != grade $ GRADE )
# ROC曲线
logit.pred2 <- predict ( grade.logit , grade , type = "response" )
lda.pred2 <- predict ( lda.fit , grade ) $ posterior [ , 2 ]
qda.pred2 <- predict ( qda.fit , grade ) $ posterior [ , 2 ]
bayes.pred2 <- predict ( bayes.fit , grade , type = "raw") [ , 2 ]
library ( ROCR )
rocplot <- function ( pred , truth , ... ) {
        predob <- prediction ( pred , truth )
        perf <- performance ( predob , "tpr", "fpr" )
        plot ( perf , ... )
        auc <- performance ( predob , "auc" )
        auc <- unlist ( slot ( auc , "y.values" ) )
        auc <- round ( auc , 4 ) #保留4位小�?
        text ( x = 0.8 , y = 0.1 , labels = paste ( "AUC =" , auc ) )
        }
par ( mfrow = c ( 2 , 2 ) )
y <- grade $ GRADE
rocplot ( logit.pred2 , y , main = "Logit" )
rocplot ( lda.pred2 , y , main = "LDA" )
rocplot ( qda.pred2 , y , main = "QDA" )
rocplot ( bayes.pred2 , y , main = "Naive Bayes" )
