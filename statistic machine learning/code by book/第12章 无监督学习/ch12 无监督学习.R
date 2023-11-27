
# ch12 R代码

# 12.6.1 聚类分析：移动通信用户细分
# 数据描述
mobile <- read.csv ( "mobile.csv" , header = TRUE , stringsAsFactors = FALSE )
dim ( mobile )
summary ( mobile )
colnames ( mobile ) <- c ( "CustomerID" , "Peak" , "OffPeak" , 
                           "Weekend" , "International" , "Total" , "Average" )
str ( mobile )
boxplot ( mobile [ - 1 ] , notch = TRUE , col = "gray" , main = "Box Plot Communication Data" )
# 系统聚类
mobile.std <- scale ( mobile [ - 1 ] , center = TRUE , scale = TRUE ) # 数据标准化
dist.mobile.std <- dist ( mobile.std , method = "euclidean" , p = 2 ) # 计算各样本间的欧式距离
mobile.hc <- hclust ( dist.mobile.std , method = "ward.D" ) # 按照“ward”方式进行聚类
plot ( mobile.hc , labels = FALSE , hang = - 1 , cex = 0.7 ) # 画出聚类结果
rect.hclust ( mobile.hc , k = 3 , border = "red" ) # 用矩形画出聚类3类的区域
mobile.hc $ result <- cutree ( mobile.hc , k = 3 ) # 聚成3类
table ( mobile.hc $ result )
mds <- cmdscale ( dist.mobile.std , k = 2 , eig = TRUE ) # 对数据降维处理
x <- mds $ points [ , 1 ] ; y <- mds $ points [ , 2 ]
library ( ggplot2 )
p <- ggplot ( data.frame ( x , y ) , aes ( x , y ) )
attach ( mobile.hc )
p + geom_point ( alpha = 0.7 , aes ( colour = factor ( result ) , shape = factor ( result ) ) )
detach ( mobile.hc )
# K均值聚类
mobile.kmc <- kmeans ( mobile.std , 3 ) # 依据系统聚类的结果，这里确定聚成3类
names ( mobile.kmc )
mobile.kmc $ centers # 查看聚类中心
mobile.new <- cbind ( mobile [ - 1 ] , mobile.kmc $ cluster )
aggregate ( .~ mobile.kmc $ cluster , data = mobile.new [ - 7 ] , mean ) # 按类分组求均值
pct <- round ( ( size / 3395 ) * 100 , digits = 2 )
lbs <- paste ( c ( "第1类" , "第2类" , "第3类" ) , " ( ",size," , ",pct," % ) " , sep = " " )
pie ( table ( mobile.kmc $ cluster ) ,
      labels = lbs ,
      col = grey.colors ( 3 ) ,
      main = "Pie Chart with Percentages" ) # 将结果画成饼图
attach ( mobile.kmc )
p + geom_point ( alpha = 0.7 , aes ( colour = factor ( cluster ) , shape = factor ( cluster ) ) )
detach ( mobile.kmc )

# 12.6.2 主成分分析：农村居民消费水平评价
library ( xlsx )
library ( psych )
data0603 <- "C:/data0603.xlsx"
consume <- read.xlsx ( data0603 , 1 ) # 读取数据
fa.parallel ( consume [ 2 : 9 ] , fa = "pc" ) # 用Kaiser-Harris准则和碎石图选择主成分个数
con.pr <- principal ( consume [ , 2 : 9 ] , nfactors = 1 , rotate = "none" , scores = TRUE )
con.pr
con.pr $ values

# 12.6.3 因子分析：市场调查
library ( xlsx )
data0702 <- "C:/data0702.xlsx"
score <- read.xlsx ( data0702 , 1 ) # 读取数据
apply ( score , 2 , mean )
apply ( score , 2 , sd )
R <- cor ( score )
R
fa.parallel ( score , fa = "both" )
fa <- factanal ( score , 2 , scores = "regression" , rotation = "varimax" )
fa

# 12.6.4 典型相关分析：职业满意度与职业特性的关系
cancorr <- function ( R , p , q ) {
        a <- p + 1 ; b <- p + q
        R11 <- R [ 1 : p , 1 : p ]
        R12 <- R [ 1 : p , a : b ]
        R21 <- R [ a : b , 1 : p ]
        R22 <- R [ a : b , a : b ] # 提取各部分相关系数矩阵
        M1 <- solve ( R11 ) %*% R12 %*% solve ( R22 ) %*% R21
        M2 <- solve ( R22 ) %*% R21 %*% solve ( R11 ) %*% R12 # 计算M1和M2
        r <- min ( p , q )
        eig1 <- eigen ( M1 )
        eig2 <- eigen ( M2 ) # 求M1和M2特征根和特征向量
        eig <- sqrt ( eig1 $ values ) [ 1 : r ]
        rownamea <- paste ( "X" , 1 : p , sep = " ")
        colnamea <- paste ( "U" , 1 : r , sep = " " )
        rownameb <- paste ( "Y" , 1 : q , sep = " " )
        colnameb <- paste ( "V" , 1 : r , sep = " " )
        A <- matrix ( 0 , nrow = p , ncol = r , dimnames = list ( rownamea , colnamea ) )
        B <- matrix ( 0 , nrow = q , ncol = r , dimnames = list ( rownameb , colnameb ) )
        for ( i in 1 : r ) A [ , i ] <- eig1 $ vectors [ , i ]
        for ( i in 1 : r ) B [ , i ] <- eig2 $ vectors [ , i ]
        list ( cor = eig , xcoef = A , ycoef = B )
}
library ( xlsx )
data0902 <- "C:/data0902.xlsx"
occupation <- read.xlsx ( data0902 , 1 )
R <- as.matrix ( occupation )
cca <- cancorr ( R , 5 , 7 )
cca
cacoef.test <- function ( cor , n , p , q , alpha = 0.05 ) {
        r <- length ( cor ) # 确定特征根个数r 
        Q <- rep ( 0 , r ) 
        lambda <- 1
        for ( i in r : 1 ) {
                lambda <- lambda * ( 1 - cor [ i ] ^ 2 )
                Q [ i ] <- - ( n - i + 1 - 1 / 2 * ( p + q + 3 ) ) * log ( lambda )
        } # 构建r个检验统计量
        for (i in 1 : r ) {
                a <- 1 - pchisq ( Q [ i ] , ( p - i + 1 ) * ( q - i + 1 ) )
                if ( a > alpha ) {
                        k <- i - 1
                        break
                }
        }
        k
}
source ( "C:/cacoef.test.R" )
cacoef.test ( cca $ cor , 784 , 5 , 7 )


