
# ch9 R代码

# 9.6.2 描述性统计
library ( glmpath )
data ( heart.data )
attach ( heart.data )
heart <- data.frame ( cbind ( as.matrix ( heart.data $ x ) , y ) ) # 将数据转换成数据框
detach ( heart.data )
summary ( heart )
heart $ famhist <- as.factor ( heart $ famhist )
heart $ y <- as.factor ( heart$y )
table ( heart $ y )
set.seed ( 1 )
index <- sample ( nrow ( heart ) , 300) # 抽取300个样本作为训练集
train <- heart [ index , ]
test <- heart [ - index , ]
table ( train $ y )
table ( test $ y )

# 9.6.3分类树
library ( tree )
tree.heart <- tree ( y ~. , train )
summary ( tree.heart )
plot ( tree.heart )
text ( tree.heart , pretty = 0 )
tree.heart
tree.pred <- predict ( tree.heart , test , type = "class" )
table ( tree.pred , test $ y )
( 78 + 35 ) / 162
set.seed ( 1 )
cv.heart <- cv.tree ( tree.heart , FUN = prune.misclass )
cv.heart
plot ( cv.heart $ size , cv.heart $ dev , type = "b" , xlab = "Tree size" , ylab = "Error" )
prune.heart <- prune.misclass ( tree.heart , best = 6)
plot ( prune.heart )
text ( prune.heart , pretty = 0)
cv.tree.pred <- predict ( prune.heart , test , type = "class" )
table ( cv.tree.pred , test $ y )
( 94 + 25 ) / 162

# 9.6.4 Bagging
library ( randomForest )
set.seed ( 1 )
bag.heart <- randomForest ( y~. , data = train , mtry = ncol ( heart ) – 1 , importance = TRUE )
bag.pred.heart <- predict ( bag.heart , newdata = test , type = "class" )
table ( bag.pred.heart , test $ y )
( 92 + 22 ) / 162

# 9.6.5 随机森林
set.seed ( 1 )
rf.heart <- randomForest ( y~. , data = train , importance = TRUE )
rf.pred.heart <- predict ( rf.heart , newdata = test , type = "class" )
table ( rf.pred.heart , test $ y )
( 92 + 23 ) / 162
importance ( rf.heart )
varImpPlot ( rf.heart )

# 9.6.6 Boosting
# Adaboost
library ( gbm )
train.a <- train
train.a $ y <- as.numeric ( levels ( train.a $ y ) ) [ train.a $ y ]
test.a <- test
test.a $ y <- as.numeric ( levels ( test.a $ y ) ) [ test.a $ y ]
set.seed ( 1 )
adaboost.heart <- gbm ( y~. , data = train.a , distribution = "adaboost",
                        n.trees = 5000 , interaction.depth = 4 )
summary ( adaboost.heart )
plot ( adaboost.heart , i = "tobacco" )
adaboost.pred.heart <- predict ( adaboost.heart , newdata = test.a, 
                                 n.trees = 5000 , type = "response" )
adaboost.pred.heart <- ifelse ( adaboost.pred.heart > 0.5 , 1 , 0 )
table ( adaboost.pred.heart , test $ y )
( 93 + 24 ) / 162

# XGBoost
library ( xgboost )
data ( heart.data )
attach ( heart.data )
heart <- data.frame ( cbind ( as.matrix ( heart.data $ x ) , y ) )
detach ( heart.data )
set.seed ( 1 )
index <- sample ( nrow ( heart ) , 300 ) # 抽取300个样本作为训练集
train <- heart [ index , ]
test <- heart [ - index , ]
xgtrain_s <- Matrix::sparse.model.matrix ( y ~ . -1 , data = train )
xgtest_s <- Matrix::sparse.model.matrix ( y ~ . -1 , data = test )
dtrain <- xgb.DMatrix ( data = xgtrain_s , label = train $ y )
dtest <- xgb.DMatrix ( data = xgtest_s , label = test $ y )
set.seed ( 1 )
xgb <- xgboost ( data = dtrain , 
                 max.depth = 10 , 
                 min_child_weight = 1 ,
                 gamma = 0.1 ,
                 colsample_bytree = 0.8 ,
                 subsample=0.8 ,
                 scale_pos_weight = 1 ,
                 eta = 0.1 , eval_metric = "auc" , nround = 10000 , objective = "binary:logistic",
                 silent = TRUE)
predxgb_test <- predict ( xgb , xgtest_s )
xgbpt <- function ( p ) {
        prediction <- as.numeric ( predxgb_test > p )
        return ( table ( prediction , test $ y ) )
}
xgbpt ( 0.5 )
( 90 + 20 ) / 162
importance_matrix <- xgb.importance ( model = xgb )
print ( importance_matrix )
xgb.plot.importance ( importance_matrix = importance_matrix )

