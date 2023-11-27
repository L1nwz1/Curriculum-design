
# ch13 R代码

# 13.3.1关联规则
baskets0 <- read.csv ( "BASKETS1n.csv" ) # 读取数据
dim ( baskets0 )
colnames ( baskets0 )
summary ( baskets0 )
baskets <- baskets0 [ , - 1 : - 7 ] # 剔除无关变量
str ( baskets )
# 关联分析
install.packages ( "arules" ) # 装载"arules"包
library ( arules )
trans <- as ( baskets , "transactions" )  # 这里做转换是为方便作图需要
windows ( 6 , 4 )
itemFrequencyPlot ( trans , sup = 0.1 , topN = 11 , col = grey.colors ( 11 ) )
rules1 <- apriori ( baskets , control = list ( verbose = FALSE ) )
summary ( rules1 )
inspect ( rules1 )
rules2 <- apriori ( baskets, parameter = list ( supp = 0.03 , conf = 0.8 , target = "rules" ) , 
                    control = list ( verbose = FALSE ) )
summary ( rules2 )
rules2.sorted <- sort ( rules2 , by = "lift" ) # 参照“lift”大小对规则（默认为降序）排列
inspect ( rules2.sorted [ 1 : 5 ] ) # 展示排序后的前五条规则
# 简单可视化
library ( arulesViz )
windows ( 6 , 4 )
plot ( rules2 )
# 删除冗余的规则
subset.matrix <- is.subset ( rules2.sorted , rules2.sorted )
subset.matrix [ lower.tri ( subset.matrix , diag = T ) ] <- NA
redundant <- colSums ( subset.matrix , na.rm = T ) >= 1
which ( redundant )
rules2.pruned <- rules2.sorted [ ! redundant ]
inspect ( rules2.pruned )

# 13.3.2协同过滤算法
# 数据描述
library ( recommenderlab )
data ( MovieLense ) # 数据集的类型为realRatingMatrix
dim ( MovieLense )
hist ( getRatings ( normalize ( MovieLense ) ) , breaks = 100 )
image ( MovieLense )
# 建立推荐系统
recommenderRegistry $ get_entry_names ( )
set.seed ( 123 )
rdata <- evaluationScheme ( MovieLense , method = "split" , train = 0.7 , given = 10 )
train <- getData ( rdata , "train" ) # 表示获取训练集数据。
r.UBCF <- Recommender ( train , method = "UBCF" ) # 基于用户
r.IBCF <- Recommender ( train , method = "IBCF" ) # 基于物品
r.SVD <- Recommender ( train , method = "SVD" ) # 基于SVD
predict1 <- predict ( r.UBCF , MovieLense[1] , type = "ratings")
getRatings ( predict1 )
predict2 <- predict ( r.UBCF , MovieLense [ 1 ] , n = 5 , type = "topNList" )
as ( predict2 , "list") # 结果需转化为list表示
pred1 <- predict ( r.UBCF , getData ( rdata , "known" ) , type = "ratings" )
pred2 <- predict ( r.IBCF , getData ( rdata , "known" ) , type = "ratings" )
pred3 <- predict ( r.SVD , getData ( rdata , "known" ) , type = "ratings" )
error1 <- calcPredictionAccuracy ( pred1 , getData ( rdata , "unknown" ) )
error2 <- calcPredictionAccuracy ( pred2 , getData ( rdata , "unknown" ) )
error3 <- calcPredictionAccuracy ( pred3 , getData ( rdata , "unknown" ) )
error <- rbind ( error1 , error2 , error3 )
rownames ( error ) <- c ( "UBCF" , "IBCF" , "SVD" )
error

