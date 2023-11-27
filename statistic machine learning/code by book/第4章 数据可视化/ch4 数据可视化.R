
# ch4 R代码

# 4.1.1 快速绘图
# 散点图
library ( ggplot2 )
qplot ( carat , price , data = diamonds )
qplot ( carat , price , data = diamonds , colour = color )
qplot ( carat , price , data = diamonds , shape = cut )
qplot ( carat , price , data = diamonds , alpha = I ( 1 / 50 ) )
p <- ggplot ( diamonds , aes ( carat , price ) )
p + stat_bin2d ( bins = 100 )
p <- ggplot ( diamonds , aes ( carat , price ) )
p + geom_point ( aes ( colour = cut , shape = cut , size = depth ) , alpha = 0.6 , position = 'jitter' )
# 散点图上添加平滑曲线
qplot ( carat , price , data = diamonds , geom = c ( "point" , "smooth" ) )
p <- ggplot ( diamonds , aes ( carat , price , colour = cut ) )
p + geom_point ( alpha = 0.1 ) + geom_smooth ( )
# 条形图和箱线图
qplot ( color , data = diamonds , geom = "bar" )
qplot ( color , data = diamonds , geom = "bar" , weight = carat ) + scale_y_continuous ( "carat" )
p <- ggplot ( data = diamonds , aes ( x = color , fill = factor ( cut ) ) )
p + geom_bar ( position = 'stack' )
p + geom_bar ( position = 'dodge' )
p + geom_bar ( position = 'fill' )
p + geom_bar ( position = 'identity' , alpha = 0.3 )
qplot ( color , price / carat , data = diamonds , geom = "boxplot" )
p <- ggplot ( diamonds , aes ( color , price / carat , fill = color ) )
p + geom_boxplot ( )
p + geom_violin ( alpha = 0.8 , width = 0.9 ) + geom_jitter ( shape = 21 , alpha = 0.03 )
# 直方图和密度曲线图
qplot ( carat , data = diamonds , geom = "histogram" , binwidth = 1 )
qplot ( carat , data = diamonds , geom = "histogram" , binwidth = 0.1 )
qplot ( carat , data = diamonds , geom = "histogram" , fill = cut )
qplot ( carat , data = diamonds , geom = "density" , colour = cut )
p <- ggplot ( diamonds , aes ( carat ) )
p + geom_histogram ( position = 'identity' , alpha = 0.3 , 
                     aes ( y = ..density.. , fill = cut ) , color = "white" ) + 
        stat_density ( geom = 'line' , position = 'identity' , aes ( colour = cut ) )
# 时间序列图
qplot ( date , uempmed , data = economics , geom = "line" )

# 4.1.2 用图层构建图像
# 数据
p <- ggplot ( mtcars , aes ( mpg , wt , colour = cyl ) ) + geom_point ( ) 
p
mtcars <- transform ( mtcars , mpg = mpg ^ 2 )
p %+% mtcars
# 一组图形属性映射
p <- ggplot ( diamonds , aes ( carat , price , colour = cut ) )
aes ( x = weight , y = height , colour = age )
aes ( weight , height , colour = sqrt ( age ) )
p <- ggplot ( mtcars )
summary ( p )
summary ( p )
p + geom_point ( colour = 'darkblue' )
p + geom_point ( aes ( colour = 'darkblue' ) )
# 统计变换
ggplot ( diamonds , aes ( carat ) ) + geom_histogram ( aes ( y = ..density.. , binwidth = 0.1 ) )
qplot ( carat , ..density.. , data = diamonds , geom = 'histogram' , binwidth = 0.1 )
# 位置调整
p <- ggplot ( mtcars )
p + geom_bar ( aes ( x = am , fill = factor ( cyl ) ) , colour = 'black' ) +
        scale_fill_manual ( values = gray ( 1 : 3 / 3 ) )
p + geom_bar ( aes ( x = am , fill = factor ( cyl ) ) , colour = 'black' , position = 'dodge' ) + 
        scale_fill_manual ( values = gray ( 1 : 3 / 3 ) )
p + geom_bar ( aes ( x = am , fill = factor ( cyl ) ) , colour = 'black' , position = 'fill' ) + 
        scale_fill_manual ( values = gray ( 1 : 3 / 3 ) )

# 4.1.3分面
# 网格分面
mpg2 <- subset ( mpg , cyl != 5 & drv %in% c ( '4' , 'f' ) )
qplot ( cty , hwy , data = mpg2 ) + facet_null ( )
qplot ( cty , hwy , data = mpg2 ) + facet_grid ( . ~ cyl )
qplot ( cty , data = mpg2 , geom = 'histogram' , binwidth = 2 ) + facet_grid ( cyl ~. )
qplot ( cty , hwy , data = mpg2 ) + facet_grid ( drv ~ cyl )
# 封装分面
ggplot ( mpg , aes ( displ , hwy ) ) + geom_point ( ) + facet_wrap ( ~ class , nrow = 4 )

# 4.1.4主题
# 内置主题
p <- ggplot ( mtcars )
p + geom_point ( aes ( x = disp , y = mpg ) )
p
p + geom_point ( aes ( x = disp , y = mpg ) ) + theme_bw ( )

# 4.1.5图形注解
( unemp <- qplot ( date , unemploy , data = economics , geom = 'line' ,
                   xlab = ' ' , ylab = 'No. unemploy(1000s)' ) )
presidential <- presidential [ - ( 1 : 3 ) , ]
yrange <- range ( economics $ unemploy )
xrange <- range ( economics $ date )
unemp + geom_vline ( aes ( xintercept = as.numeric ( start ) ) , data = presidential )
library ( scales )
unemp + geom_rect ( aes ( NULL , NULL , xmin = start , xmax = end , fill = party ) , 
                    ymin = yrange [ 1 ] , ymax = yrange [ 2 ] ,
                    data = presidential , alpha = 0.2 ) +
        scale_fill_manual ( values = c ( 'blue' , 'red' ) )
last_plot ( ) + geom_text ( aes ( x = start , y = yrange [ 1 ] , label = name ) ,
                            data = presidential , size = 3 , hjust = 0 , vjust = 0 )

# 4.2.1 安装
devtools::install_github ( 'madlogos/recharts' )

# 4.2.2使用
library ( recharts )
# 散点图
e1 <- echartr ( iris , x = SepalWidth , y = PetalWidth , series = Species )
e1
# 气泡图
e2 <- echartr ( iris , PetalLength , PetalWidth , weight = SepalWidth , type = 'bubble' )
e2
# 条图
library ( data.table )
titanic <- melt ( apply ( Titanic , c ( 1 , 4 ) , sum ) )
names ( titanic ) <- c ( 'Class' , 'Survived' , 'Count' )
View ( titanic )
e3 <- echartr ( titanic , Class , Count , Survived , type = 'hbar' )
e3
e4 <- echartr ( titanic , Class , Count , Survived , type = 'hbar' , subtype = 'stack' )
e4
e5 <- echartr ( titanic , Class , Count , Survived , type = 'vbar' )
e5
e6 <- echartr ( titanic , Class , Count , Survived , type = 'vbar' , subtype = 'stack' )
e6
# K线图
e7 <- echartr ( stock , as.character ( date ) , c ( open , close , low , high ) , type='candlestick') %>% 
        setXAxis ( name = 'Date' , axisLabel = list ( rotate = 25 ) ) %>%
        setYAxis ( name = "StockPrice" )
e7
# 词云
getBaiduHot <- function ( url , top = 20 , HTMLencoding = NULL ) {
        # readLines ( )函数从url链接读取一些文本信息
        # paste0 ( )函数用于连接字符串，此处连接readLines ( )返回的文本。
        baiduhot <- paste0 ( readLines ( url ) , collapse = " " )
        # gsub ( )函数按照第一个参数的模式在字符串baiduhot中进行匹配，返回“( )”中的值，并赋值给charset。
        charset <- gsub ( '^.+charset=([[:alnum:]-]+?)[^[:alnum:]-].+$' , "\\1" , baiduhot )
        if ( is.null ( HTMLencoding ) ) if ( ! is.null ( charset ) ) HTMLencoding <- charset
        # str_conv ( )函数指定字符串的编码类型，此处将字符串baiduhot的编码类型指定为HTMLencoding代表的类型。
        baiduhot <- stringr::str_conv ( baiduhot , HTMLencoding )
        # gsub ( )函数依据第一个参数的模式匹配和提取baiduhot中的list-title标签内热度词的标题和数量，并赋值给hotword。
        hotword <- gsub ( ".+?<a class=\"list-title\"[^>]+?>([^<>]+?)</a>.+?<span class=\"icon-(rise|fair|fall)\">(\\d+?)</span>.+?" , "\\1\t\\3\t" , baiduhot )
        # enc2native ( )函数将字符串编码设置为本地编码格式
        hotword <- enc2native ( gsub ( "^(.+?)\t{4,}.+$" , "\\1" , hotword ) )
        hotword <- t ( matrix ( unlist ( strsplit ( hotword , "\t" ) ) , nrow=2 ) )
        hotword <- as.data.frame ( hotword , stringsAsFactors = FALSE )
        names ( hotword ) <- c ( "Keyword" , "Freq" )
        hotword $ Freq <- as.numeric ( hotword $ Freq )
        hotword <- hotword [ order ( hotword $ Freq , decreasing = TRUE ) , ]
        return ( hotword [ 1 : top , ] )
}
hotword <- getBaiduHot ( "http://top.baidu.com/buzz?b=1" , HTMLencoding = 'GBK' )
e8 <- echartr ( hotword , Keyword , Freq , type = 'wordCloud' ) %>% 
        setTitle ( 'Baidu Hot Word Top20' , as.character ( Sys.time ( ) ) )  
e8
# 热力图
df1 <- data.frame ( lng = 200 + rnorm ( 100 , 0 , 1 ) * 100 , 
                    lat = 200 + rnorm ( 100 , 0 , 1 ) * 100 , y = abs ( rnorm ( 100 , 0 , 1 ) ) )
df2 <- data.frame ( lng = rnorm ( 200 , 0 , 1 ) * 100 , 
                    lat = - 100 + rnorm ( 200 , 0 , 1 ) * 100 , y = abs ( rnorm ( 200 , 0 , 1 ) ) )
df3 <- data.frame ( lng = 40 + rnorm ( 50 , 0 , 1 ) * 300 , 
                    lat = rnorm ( 50 , 0 , 1 ) * 10 , y = abs ( rnorm ( 50 , 0 , 1 ) ) )
data <- rbind ( df1 , df2 , df3 )
str ( data )
e9 <- echartr ( data , lng = lng , lat = lat , y = y , type = 'heatmap' ) %>% setTitle ( "Heatmap" )
e9
# 雷达图
area <- c ( '溪东' , '洪文' , '鼓浪屿' , '湖里中学' )
indicators <- c ( 'PM2.5细颗粒物' , 'PM10可吸入颗粒物' , 
                  'O3臭氧1小时平均' , 'O3臭氧8小时平均' )
data <- matrix ( c ( 6 , 13 , 32 , 51 , 3 , 17 , 42 , 52 , 14 , 24 , 43 , 
                     52 , 7 , 16 , 44 , 53 ) , 4 , 4 , byrow = TRUE )
data <- as.data.frame ( data )
rownames ( data ) <- area
colnames ( data ) <- indicators
data $ area <- rownames ( data )
data <- data.table::melt ( data , id.vars = 'area' )
names ( data ) <- c ( 'area' , 'indicators' , 'value' )
e10 <- echartr ( data , indicators , value , series = area , type = 'radar' , sub = 'fill' ) %>% 
setTitle ( '2017/8/27: PM2.5细颗粒物 vs PM10可吸入颗粒物 vs O3臭氧1小时平均 vs O3臭氧8小时平均' ) 
e10
e11 <- echartr ( data , indicators , value , facet = area , type = 'radar' , sub = 'fill' ) %>% 
        setTitle ( '2017/8/27:PM2.5细颗粒物 vs PM10可吸入颗粒物 vs O3臭氧1小时平均 vs O3臭氧8小时平均' ) %>% 
        setPolar ( type = 'circle' )
e11

