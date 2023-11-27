
# ch3 R代码

# 3.6.1数据集的基本操作
library ( dplyr )
library ( hflights )
hflights_df <- tbl_df ( hflights )
# 数据筛选
filter ( hflights_df , Month == 1 , DayofMonth == 1 ) # 且的关系
filter ( hflights_df , Month == 1 | Month == 2 ) # 或的关系
slice ( hflights_df , 1 : 10 )
sample_n ( hflights_df , 10 ) # 随机从数据集中选取10个样本
sample_frac ( hflights_df , 0.1 ) # 随机从数据集中选取10%的样本
# 数据排序
arrange ( hflights_df , DayofMonth , Month , Year )
arrange ( hflights_df , desc ( ArrDelay) ) 
# 数据选择
select ( hflights_df , Year , Month , DayofMonth )
select ( hflights_df , Year : DayofMonth ) 
select ( hflights_df , - ( Year : DayofMonth ) ) 
# 数据变形
mutate ( hflights_df , gain = ArrDelay - DepDelay , gain_per_hour = gain / ( AirTime / 60 ) )
# 汇总操作
summarise ( hflights_df , a = mean ( DepDelay , na.rm = TRUE ) , b = max ( DepDelay ) ,
            c = n_distinct ( DepDelay ) )
# 数据分组
planes <- group_by ( hflights_df , TailNum )
delay <- summarise ( planes , count = n ( ) , dist = mean ( Distance , na.rm = TRUE ) , 
                     delay = mean ( ArrDelay , na.rm = TRUE ) )
delay2 <- filter ( delay , count > 20 , dist < 2000 )
# 变量重命名
rename ( hflights_df , Year1 = Year )
# 其他小函数
summarise ( hflights_df , a = n_distinct ( DepDelay ) )
# 管道函数
hflights_df %>%
        group_by ( Month , DayofMonth ) %>%
        select ( ArrDelay , DepDelay ) %>%
        summarise ( arr = mean ( ArrDelay , na.rm = TRUE ) ,dep = mean ( DepDelay , na.rm = TRUE ) ) %>%
        filter ( arr > 30 | dep > 30 )

# 3.6.3连接数据库数据
my_db <- src_mysql ( host = "blah.com" , user = "hadley" , password = "pass" )
my_tbl <- tbl ( my_db , "my_table" )


