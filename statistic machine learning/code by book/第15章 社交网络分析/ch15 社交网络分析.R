
# ch15 R代码

# 15.6.1 网络的基本操作
library(igraph)
g <- graph.formula ( 1 - 2 , 1 - 3 , 2 - 3 , 2 - 4 , 3 - 5 , 4 - 5 , 4 - 6 ,
                     4 - 7 , 5 - 6 , 6 - 7 ) # 创建一个图对象g
V ( g ) # 返回图对象g的节点
E ( g ) # 返回图对象g的边
str ( g ) # 返回图对象g的结构
plot ( g ) 
dg <- graph.formula ( 1 -+ 2 , 1 -+ 3 , 2 ++ 3 ) # 创建一个图对象dg
plot ( dg ) 
dg <- graph.formula ( Sam -+ Mary , Sam -+ Tom , Mary ++ Tom )
E ( dg ) # 获得边
get.adjacency ( g ) # 获得邻接矩阵
vcount ( g ) # 计算节点数目
ecount ( g ) # 计算边数目
h <- induced.subgraph ( g , 1:5 ) # 获得子图
str ( h ) # 返回子图h的结构
h <- g + vertices ( c ( 6 , 7 ) ) # 增加节点6、7
g <- h + edges ( c ( 4 , 6 ) , c ( 4 , 7 ) , c ( 5 , 6 ) , c ( 6 , 7 ) ) 
# 增加4和6、4和7、5和6、6和7之间的边
library ( sand )
g.l <- graph.lattice ( c ( 5 , 5 , 5 ) ) # 生成了一个5x5x5网格图
par ( mfrow = c ( 1 , 2 ) ) # 绘图 
plot ( g.l , layout = layout.circle ) # layout的参数为layout.circle
title ( "5x5x5 Lattice" )
plot ( g.l, layout = layout.fruchterman.reingold ) # layout参数为layout.fruchterman.reingold
title ( "5x5x5 Lattice" )
V ( dg ) $ name  # 图对象dg的name属性为1、2、3
V ( dg ) $ gender <- c ( "M" , "F" , "M" ) # 赋予图对象dg的gender属性为M、F、M
V ( g ) $ color <- "red" # 赋予图对象dg的color属性为red

# 15.6.2 “空手道俱乐部网络”特征分析
library ( igraphdata )
library ( sand ) 
library ( network )
data ( karate ) # 我们分析的即为karate数据
set.seed ( 42 ) 
l <- layout.kamada.kawai ( karate ) # 确定页面布局
plot ( karate , layout = l , vertex.label = NA ) 
V ( karate ) $ label <- sub ( "Actor" , " " , V ( karate ) $ name ) # 确定标签
# 确定领导者的形状与其他成员不同，领导者为长方形，其他成员为圆形
V ( karate ) $ shape <- "circle"  
V ( karate ) [ c ( "Mr Hi" , "John A" ) ] $ shape <- "rectangle" 
# 用不同颜色表示不同的派别，分别为红色和蓝色
V ( karate ) [ Faction == 1 ] $ color <- "red" 
V ( karate ) [ Faction == 2 ] $ color <- "dodgerblue"
# 节点的面积正比于节点的强度
V ( karate ) $ size <- 4 * sqrt ( graph.strength ( karate ) )
V ( karate ) $ size2 <- V ( karate ) $ size * .5
# 将共同活动的数量设定为边的权重/粗细
E ( karate ) $ width <- E ( karate ) $ weight
# 使用不同的颜色来代表边是内部之间或派别之间
F1 <- V ( karate ) [ Faction == 1 ]
F2 <- V ( karate ) [ Faction == 2]
E ( karate ) [ F1 %--% F1 ] $ color <- "pink" # F1内部的边颜色为粉色
E ( karate ) [ F2 %--% F2 ] $ color <- "lightblue" # F2内部的边颜色为蓝色
E ( karate ) [ F1 %--% F2 ] $ color <- "yellow" # 派别之间的边颜色为黄色
V ( karate ) $ label.dist <- ifelse ( V ( karate ) $ size >= 10 , 0 , 0.75 ) 
# 较小节点的标签位置偏移量（初始为0）
plot ( karate , layout = l ) 
ecount ( karate ) # 返回该网络的边数目
vcount ( karate ) # 返回该网络的节点数目
A <- get.adjacency ( karate , sparse = FALSE ) # 得到karate的邻接矩阵
library ( network )
g <- network :: as.network.matrix ( A ) # sna包中所用的图对象的格式
library ( sna )
sna :: gplot.target ( g , degree ( g ) , main= "Degree" ,
                      circ.lab = FALSE , circ.col = "skyblue" ,
                      usearrows = FALSE ,
                      vertex.col = c ( "blue" , rep ( "red" , 32 ) , "yellow" ) ,
                      edge.col = "darkgray" )
table ( sapply ( cliques ( karate ) , length ) ) # 返回该图对象各个尺寸的团的数目
cliques ( karate ) [ sapply ( cliques ( karate ) , length ) == 5 ] # 返回尺寸为5的团
cores <- graph.coreness ( karate )
sna :: gplot.target ( g , cores , circ.lab = FALSE , 
                      circ.col = "skyblue" , usearrows = FALSE , 
                      vertex.col = cores , edge.col = "darkgray" )
detach( "package:sna" )
ego.instr <- induced.subgraph ( karate , neighborhood ( karate , 1 , 1 ) [[ 1 ]] ) 
# 得到节点1的个体中心网络
ego.admin <- induced.subgraph ( karate , neighborhood ( karate , 1 , 34 ) [[ 1 ]] ) 
# 得到节点34的个体中心网络
graph.density ( karate ) # karate的密度
graph.density ( ego.instr ) # ego.instr的密度
graph.density ( ego.admin ) # ego.admin的密度
transitivity ( karate ) # karate的聚类系数
transitivity ( karate , "local" , vids = c ( 1 , 34 ) ) # 局部聚类系数
kc <- fastgreedy.community ( karate ) # 使用层次聚类方法将karate进行分割
length ( kc ) # 得到3个社团
sizes ( kc ) # 每个社团所包含的节点数
membership ( kc ) # 分析社团的成员归属
plot ( kc , karate ) 
library ( ape )
dendPlot ( kc , mode = "phylo" ) 
data ( karate )
nv <- vcount ( karate ) # 得到karate的节点数目
ne <- ecount ( karate ) # 得到karate的边数目
degs <- degree ( karate ) # 得到karate的每个节点的度
ntrials <- 1000  # 模拟1000次
# 生成具有相同阶数和尺寸的经典随机图
num.comm.rg <- numeric ( ntrials )
for (i in ( 1:ntrials ) ) {
        g.rg <- erdos.renyi.game ( nv , ne, type = "gnm" )
        c.rg <- fastgreedy.community ( g.rg )
        num.comm.rg [ i ] <- length ( c.rg )
}
# 按照给定度序列生成广义随机图
num.comm.grg <- numeric ( ntrials )
for (i in ( 1:ntrials ) ) {
        g.grg <- degree.sequence.game ( degs , method = "vl" )
        c.grg <- fastgreedy.community ( g.grg )
        num.comm.grg [ i ] <- length ( c.grg )
}
# 使用并列的条形图对结果进行概括和对比
rslts <- c ( num.comm.rg , num.comm.grg )
indx <- c ( rep ( 0 , ntrials ) , rep ( 1 , ntrials ) )
counts <- table ( indx , rslts ) / ntrials
barplot ( counts , beside = TRUE , col = c ( "blue" , "red" ) ,
          xlab = "Number of Communities" , 
          ylab = "Relative Frequency" ,
          legend = c ( "Fixed Size" , "Fixed Degree Sequence" ) )

# 15.6.3 关联网络推断
library ( sand )
rm ( list = ls ( ) ) # 将现有对象清除
data ( Ecoli.data ) # 我们本节的研究对象：Ecoli.data
ls ( ) # 得到Ecoli.data中的数据所包含的两个对象名
heatmap ( scale ( Ecoli.expr ) , Rowv = NA ) 
library ( igraph )
g.regDB <- graph.adjacency ( regDB.adj , "undirected" ) # 得到邻接矩阵
summary ( g.regDB )
plot ( g.regDB , vertex.size = 3 , vertex.label = NA ) 
mycorr <- cor ( Ecoli.expr ) # 计算所有基因对之间的相关值
z <- 0.5 * log ( ( 1 + mycorr ) / ( 1 - mycorr ) ) # 计算经过费舍尔变换的值
z.vec <- z [ upper.tri ( z ) ]
n <- dim ( Ecoli.expr ) [ 1 ]
corr.pvals <- 2 * pnorm ( abs ( z.vec ) , 0 , sqrt ( 1 / ( n-3 ) ) , lower.tail = FALSE ) # 计算p值
length ( corr.pvals ) # 检验次数
corr.pvals.adj <- p.adjust ( corr.pvals , "BH" ) # 得到修正后的p值
length ( corr.pvals.adj [ corr.pvals.adj < 0.05 ] ) # 将这个值与标准的显著性水平0.05比较，得到显著的基因对数目
# 计算相应的经验偏相关系数，并使用均值为0、方差为1 / ( n – 4 )的正态分布来近似每个系数经过费舍尔变换后的分布。
pcorr.pvals <- matrix ( 0 , dim ( mycorr ) [ 1 ] , dim ( mycorr ) [ 2 ] )
for ( i in seq ( 1 , 153 ) ) {
        for ( j in seq ( 1 , 153 ) ) {
                rowi <- mycorr [ i , - c ( i , j ) ]
                rowj <- mycorr [ j , - c ( i , j ) ]
                tmp <- ( mycorr [ i , j] - rowi * rowj ) / sqrt ( ( 1 - rowi ^ 2 ) * ( 1 - rowj ^ 2 ) )
                tmp.zvals <- 0.5 * log ( ( 1 + tmp ) / ( 1 - tmp ) )
                tmp.s.zvals <- sqrt ( n - 4 ) * tmp.zvals
                tmp.pvals <- 2 * pnorm ( abs ( tmp.s.zvals ) , 0 , 1 , lower.tail = FALSE )
                pcorr.pvals[i, j] <- max(tmp.pvals)
        }
}
# 如前对多重检验进行修正
pcorr.pvals.vec <- pcorr.pvals [ lower.tri ( pcorr.pvals ) ]
pcorr.pvals.adj <- p.adjust ( pcorr.pvals.vec , "BH" )
pcorr.edges <- ( pcorr.pvals.adj < 0.05 ) 
# 将这个值与标准的显著性水平0.05比较，得到显著的基因对
length ( pcorr.pvals.adj [ pcorr.edges ] ) # 显著的基因对数目
pcorr.A <- matrix ( 0 , 153 , 153 )
pcorr.A [ lower.tri ( pcorr.A ) ] <- as.numeric ( pcorr.edges ) 
g.pcorr <- graph.adjacency ( pcorr.A , "undirected" ) # 得到邻接矩阵
str ( graph.intersection ( g.regDB , g.pcorr , byname = FALSE ) ) 
# 在25条边中，其中4条属于生物学文献中已经发现的关系
library ( huge )
set.seed ( 1 )
huge.out <- huge ( Ecoli.expr ) # 生成最初的估计集合
huge.opt <- huge.select ( huge.out , criterion = "ric" ) # 使用第一种方法：信息准则
summary ( huge.opt$refit ) # 得到一个空图
huge.opt <- huge.select ( huge.out , criterion = "stars") # 使用子采样准则
g.huge <- graph.adjacency ( huge.opt $ refit , "undirected" ) # 得到邻接矩阵
summary ( g.huge ) # 得到了一个比较稠密的图
str ( graph.intersection ( g.pcorr , g.huge ) ) # 得到结构化数据
str ( graph.intersection ( g.regDB , g.huge , byname = FALSE ) ) # 包含了生物学文献中发现的22条边

