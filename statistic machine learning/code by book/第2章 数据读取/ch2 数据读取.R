
# ch2 R代码

# 2.1.1 直接输入数据
# c ( ) 函数
x <- c ( 1 , 2 , 3 , 4 )
x
y <- c ( "a" , "b" , "c" )
y
# scan()函数
x <- scan ( )
x <- scan ( file = "dat.txt" )
x <- scan ( file = "dat.txt" , sep = "," )

# 2.1.2 读R包中的数据
data ( package = "SemiPar" )
data ( copper , package = "SemiPar" )
library ( SemiPar )
data ( )
data ( fossil )

# 2.1.3 从外部文件读入数据
# 读入文本文件
s1 <- read.table ( "student.txt" )
s1
s2 <- read.table ( "student.txt" , header = T )
s2
# 读入EXCEL格式数据
S2 <- read.csv ( file = "student.csv" )
S2
# 读入SQL Server数据库数据
library ( RODBC )
odbcDataSources ( )
conn <- odbcConnect ( 'SQLServer' , uid = 'sa' , pwd = 'ok' )
result <- sqlQuery ( conn , 'select * from student' )
result
odbcClose ( conn )
conn <- odbcConnect ( "SQLServer" , uid = "sa" , pwd = "ok" , case = "tolower" )
# 读入其他格式数据
install.packages ( "foreign" )
library ( foreign )
# SAS数据
read.xport ( "dataname.xpt" ) # 读入SAS格式文件
# SPSS数据
read.spss ( "dataname.sav" ) # 读入SPSS格式文件
install.packages ("Hmisc")  # 安装Hmisc包
library ( Hmisc ) # 载入Hmisc包
mydataframe <- spss.get ( "dataname.sav" , use.value.labels = TRUE )
# Epi info数据
read.epiinfo ("d:/ttt.rec" ) -> ttt
# Stata数据:
mean ( data$age ) # 计算数据集data中的变量age的均数

# 2.1.4 批量读入数据
for ( id in 2004 : 2013 ) {
        Id <- paste ( "DATA" , id , sep = "" )
        dat <- read.csv ( paste ( Id , ".csv" , sep = "" ) , header = T , sep = "," )
}

# 2.1.5 R读取文件的几个常错的问题
# 编码问题
dat_ID <- read.csv ( file = "dataID.csv" , header = T , sep = ";" )
dat_ID <- read.table ( file = "dataID2.txt" , header = T , sep = "\t" , fileEncoding = "UTF-16" )
# 分隔符问题
dat <- read.csv ( file = "DATA2013.csv" , header = T , sep = ";" )
dim ( dat )
head ( dat )
dat <- read.csv ( file = "DATA2013.csv" , header = T , sep = "," )
dim ( dat )
head ( dat )

# 2.2 写出数据
write.table ( S2 , "S2.txt" )
Write.table ( S2 , "E:\\R\\data\\S2.csv" )

