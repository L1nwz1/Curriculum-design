
# ch16 R代码

# 16.1 提高R语言的计算速度
library ( compiler )
f <- function ( x ) {
        s <- 0
        for ( y in x ) s <- s + y
        return ( s )
}
fc <- cmpfun ( f )
x <- 1 : 10000000
system.time ( f ( x ) )
system.time ( fc ( x ) )

# 16.2 R语言的并行计算
# 例16.1（1）
cl <- makeCluster ( getOption ( "cl.cores" , 2 ) )
xx <- 1 : 2
yy <- 3 : 4
clusterExport ( cl , c ( "xx" , "yy" ) )
clusterCall ( cl , function ( z ) xx + yy + sin(z) , pi )
stopCluster ( cl )
# 例16.1（2）
clusterEvalQ ( cl , {
        # set up each worker. Could also use clusterExport ( )
        library ( boot )
        cd4.rg <- function ( data , mle ) MASS :: mvrnorm ( nrow ( data ) , mle$m , mle$v )
        cd4.mle <- list ( m = colMeans ( cd4 ) , v = var ( cd4 ) )
})
# 例16.1（3）
fxy <- function ( x , y ) {
        x ^ 2 + sin ( y ) – 1
}
Z <- clusterApply ( cl , 1 : 10 , fxy , y = 2 )
z <- unlist ( Z )
z
# 例16.1（4）
M <- matrix ( rnorm ( 50000000 ) , 100 , 500000 )
Mysort <- function ( x ) {
        return ( sort ( x ) [ 1 : 10 ] )
}
do_apply <- function ( M ) {
        return ( apply ( M , 2 , mysort ) )
}
do_parallel <- function ( M , ncl ) {
        cl <- makeCluster ( getOption ( "cl.cores" , ncl ) )
        ans <- parApply ( cl , M , 2 , mysort )
        stopCluster ( cl )
        return ( ans )
}
system.time ( ans <- do_apply ( M ) )
system.time ( ans2 <- do_parallel ( M , 2 ) )
# 例16.1（5）
set.seed ( 123 )
clusterCall ( cl , function ( ) rnorm ( 1 ) )
set.seed ( 123 )
clusterCall ( cl , function ( ) rnorm ( 1 ) )
clusterSetRNGStream ( cl , iseed = 123 ) # 给集群内的随机数的生成设置随机种子
clusterCall ( cl , function ( ) rnorm ( 1 ) )
clusterSetRNGStream ( cl , iseed = 123 ) # 给集群内的随机数的生成设置随机种子
clusterCall (cl , function ( ) rnorm ( 1 ) )

# 例16.2（1）
library ( foreach )
library ( doParallel )
cl <- makeCluster ( getOption ( "cl.cores" , 2 ) )
registerDoParallel ( cl )
# ……
stopCluster ( cl )

X <- matrix ( 0 , nr = 10 , nc = 10 )
for ( i in 1 : 10 ) {
        X [ i , ] <- 5 * i - rnorm ( 10 , mean = i )
}
# 改写成foreach为
X <- foreach ( i = 1 : 10 , .combine = 'rbind' ) %do% { 5 * i – rnorm ( 10 , mean = i ) }
# 改写为并行计算格式为
X <- foreach ( i = 1 : 10 , .combine = 'rbind' ) %dopar% { 5 * i – rnorm ( 10 , mean = i ) }
# 例16.2（2）
cfun <- function ( x , y ) {
        c ( x [ 1 ] , y [ 2 ] )
}
foreach ( i = 1 : 3 , .combine = 'cbind' ) %do% { z <- i : ( i + 5 ) }
foreach ( i = 1 : 3 , .combine = 'rbind' ) %do% { z <- i : ( i + 5 ) }
foreach ( i = 1 : 3 , .combine = 'cfun' ) %do% { z <- i : ( i + 5 ) }
# 例16.2（3）
library ( iterators )
foreach ( i2 = iter ( list ( x = 1 : 3 , y = 10 , z = c ( 7 , 8 ) ) ) , .combine = 'c' ) %do% { i2 }
# 例16.2（4）
# a quick sort function
qsort <- function ( x ) {
        n <- length ( x )
        if ( n == 0 ) {
                x
        } else {
                p <- sample ( n , 1 )
                # 统计比x [ p ] 小的值
                smaller <- foreach ( y = x [ -p ] , .combine = c ) %:% when ( y <= x [ p ] ) %do% y 
                # 统计比x [ p ] 大的值
                larger <- foreach ( y = x [ - p ] , .combine = c ) %:% when ( y > x [ p ] ) %do% y 
                c ( qsort ( smaller ) , x [ p ], qsort ( larger ) ) # 采用递归的方法排序。
        }
}
qsort ( runif ( 12) ) # 对随机生成的12个数进行排序
# 例16.2（5）
sim <- function ( x , y ) {
        10 * x + y
}
avc <- 1 : 4
bvc <- 1 : 4
x <- matrix ( 0 , length ( avc ) , length ( bvc ) )
for ( j in 1 : length ( bvc ) ) {
        for ( i in 1 : length ( avc ) )
                x [ i ,  j ] <- sim ( avc [ i ] , bvc [ j ] )
}
# 改写为foreach格式
x <- foreach ( b = bvc , .combine = 'cbind' ) %:%
        foreach ( a = avc , .combine = 'c' ) %do% {
                sim ( a , b )
}

# 16.3 HPC多线程并行计算
Args <- commandArgs ( )
T <- Args [ 6 ]
T <- as.numeric ( T )
library ( ncvreg )
library ( MASS )
source ( 'AIC_CV.R' )
source ( 'beta_calculate.R' )
source ( 'cross_log.R' )
# 相应的文件AIC_CV.R 等需要提前放在相应路径下。
nfold <- 2
n <- 100
p <- 50
ro <- 0.5
g <- 5
K <- p / g
gama <- 5
Lambda1 <- c ( 0.03 , 0.05 , 0.07 , 0.09 , 0.1 , 0.3 )
Lambda2 <- c ( 0.07 , 0.08 , 0.09 , 0.1 , 0.2 , 0.3 , 0.4 , 0.5 , 0.6 , 0.7 , 0.8 , 0.9 )
sigma <- matrix ( , g , g )
for ( ii in 1 : g ) {
        for ( jj in ii : g ) {
                sigma [ ii , jj ] <- ro ^ abs ( ii - jj )
                sigma [ jj , ii ] <- sigma [ ii , jj ]
        }
}
b <- vector ( )
b [ 1 ] <- 0
b [ 2 : 26 ] <- rep ( c ( 1 , 1 , 1 , 1 , 1 ) , each = 5 )
b [ 27 : ( p + 1 ) ] <- 0
b0 <- rep ( 0 , (p + 1 ) )
as.numeric ( Sys.time ( ) ) -> t
set.seed ( ( t - floor ( t ) ) * 1e8 -> seed )
print ( seed )
set.seed ( T * 200000 )
X <- matrix ( , n , p )
for ( i in 1 : K ) {
        X [ , ( g * ( i - 1 ) + 1 ) : ( g * i ) ] <- mvrnorm ( n = 100 , rep ( 0 , g ) , sigma )
}
X <- cbind ( 1 , X )
prob1 <- 1 / ( 1 + exp ( - X %*% b ) )
y = rbinom ( n , 1 , prob = prob1 )
A <- diag ( 1 , nc = ( p + 1 ) , nr = ( p + 1) )
A [ 2 : ( p + 1 ) , 2 : ( p + 1 ) ] <- ( cor ( X [ , 2 : ( p + 1 ) ] ) ) ^ 3
beta <- AIC_CV ( X, y , b0 , Lambda1 , Lambda2 )
# 执行的函数结果必须保存，保存在“//hpcserver-soe/HPCUserFile$/fanxinyan/”之后的文件里
write.csv ( beta , paste0 ( '//hpcserver-soe/HPCUserFile$/fanxinyan/R 并行/parallel with R/' , 'beta' , T , '.csv' ) )

