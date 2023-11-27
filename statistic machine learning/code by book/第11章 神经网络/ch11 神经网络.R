
# ch11 R代码

# 11.3.1 nnet程序包
# class.ind函数
v1 <- c ( "a" , "b" , "c" , "a" )
v2 <- c ( 1 , 2 , 2 , 3 )
class.ind ( v1 )
class.ind ( v2 )

# 11.3.4 应用案例1：利用nnet包分析纸币鉴别数据
note <- read.table ( "banknote.txt" )
names ( note ) = c ( "variance" , "skewness" , "curtosis" , "entropy" , "class" )
head ( note )
dim ( note )
table ( note $ class )
summary ( note )
regular <- function ( x ) { # 自定义regular函数，用来对数据进行归一化
        ncol <- dim ( x ) [ 2 ] - 1 
        nrow <- dim ( x ) [ 1 ] 
        new <- matrix ( 0 , nrow , ncol ) 
        for ( i in 1:ncol ) {
                max = max ( x [ , i ] )
                min = min ( x [ , i ] )
                for ( j in 1 : nrow )
                { new [ j , i ] = ( x [ j , i ] - min ) / ( max-min ) }
        }
        x [ , 1 : ( dim ( x ) [ 2 ] - 1 ) ] <- new
        return(x)
}
banknote <- regular ( note ) # 归一化处理，得到新数据集banknote
library ( nnet )
set.seed ( 11 ) # 设置随机数种子
index <- sample ( 1 : nrow ( banknote ) , round ( 0.75 * nrow ( banknote ) ) )   
# 抽取0.75的样本作为训练集，0.25的样本作为测试集
train <- banknote [ index , ]
test <- banknote [ - index , ]
r <- 1 / max ( abs ( train [ , 1 : 4 ] ) ) # 确定参数rang的变化范围
trainx <- train [ , 1 : 4 ]
trainy <- train [ , 5 ]
trainy <- class.ind ( trainy ) # 这里采用nnet的第二种格式，所以对Y做变换
testx <- test [ ,1 : 4 ] # 测试集
testy <- test [ , 5 ]
model1 <- nnet ( trainx , trainy , size = 0 , skip = T ) # 构建无隐藏层神经网络
summary ( model1 )
pred1 <- predict ( model1 , testx ) # 预测的结果为一矩阵
name <- c ( "0" , "1" ) # 为2个类别确定名称
pred <- name [ max.col ( pred1 ) ]   
# 确定最大值所在的列,根据预测结果将其转化为对应的类别名称
table ( testy , pred )
model2 <- nnet ( trainx , trainy , decay = 5e-4 , maxit = 1000 , size = 2 , rang = r )
summary ( model2 )
pred2 <- predict ( model2 , testx )    
name <- c ( "0" , "1" )    
pred <- name [ max.col ( pred2 ) ]    
table ( testy , pred )

# 11.3.5 应用案例2：利用neuralnet包分析白葡萄酒的品质
wwine1 <- read.table ( "wwine.txt" )
names ( wwine1) <- c ( "fixedacidity" , "volatileacidity" , "citricacid" , "residualsugar" , 
                       "chlorides" , "freesulfurdioxide" , "totalsulfurdioxide" , "density" ,
                       "pH" , "sulphates" , "alcohol" , "quality" )
dim ( wwine1 )
head ( wwine1 )
summary ( wwine1 )
normalize <- function ( x ) { return ( ( x - min ( x ) ) / ( max ( x ) - min ( x ) ) ) } # 归一化
wwine1 <- as.data.frame ( lapply ( wwine1 , normalize ) )
set.seed ( 11 ) # 设置随机数种子
index <- sample ( 1 : nrow ( wwine1 ) , round ( 0.75 * nrow ( wwine1 ) ) ) 
# 抽取0.75的样本作为训练集，0.25的样本作为测试集
train <- wwine1 [ index , ]
test <- wwine1 [ - index , ]
library ( grid ) 
library ( MASS )
library ( neuralnet )
formula <- ( quality ~ fixedacidity + volatileacidity + citricacid + 
                     residualsugar + chlorides + freesulfurdioxide + 
                     totalsulfurdioxide + density + pH + sulphates + alcohol )
model0 <- neuralnet ( formula , data = train , hidden = 1 , learningrate = 0.08 , 
                      algorithm = "backprop" , linear.output = F )   
( train.error <- model0 $ result.matrix [ 1 , 1 ] )
( steps <- model0 $ result.matrix [ 3 , 1 ] )
( test.error <- sum ( ( compute ( model0 , test[ , 1 : 11 ] ) $ net.result - test [ , 12 ] ) ^ 2 ) / 2 )
library ( grid )
model00 <- neuralnet ( formula , data = train , hidden = 1 )   
model00 $ result.matrix
( test.error <- sum ( ( compute ( model00 , test [ , 1 : 11 ] ) $ net.result - test [ , 12 ] ) ^ 2 ) / 2 ) 
model06 <- neuralnet ( formula , data = train , hidden = c ( 3 , 2 ) )   
model06 $ result.matrix
( test.error <- sum ( ( compute ( model06 , test [ , 1 : 11 ] ) $ net.result - test [ , 12 ] ) ^ 2 ) / 2 )
plot ( model06 , information = F )

