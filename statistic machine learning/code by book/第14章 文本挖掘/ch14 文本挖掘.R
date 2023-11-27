
# ch14 R代码

# 14.3.1 JSS_papers数据集
install.packages ( 'tm' )
install.packages ( 'XML' )
install.packages ( 'SnowballC' )
install.packages ( 'wordcloud' )
install.packages ( 'topicmodels' )
install.packages ( "corpus.JSS.papers" , repos = http://datacube.wu.ac.at/ , type = "source" )
library ( tm ) # 英文文本挖掘包
library ( XML ) # 处理HTML源码包
library ( SnowballC ) # 英文分词过程中词干处理包
library ( corpus.JSS.papers ) # 数据来源包
library ( slam ) # 简单三元组矩阵（Simple Triplet Matrix）处理包
library ( wordcloud ) # 绘制词云所用包
library ( topicmodels ) # 主题模型包
data ( 'JSS_papers' )
JSS <- JSS_papers [ JSS_papers [ , 'date' ] > '2010-08-05' , ]
JSS <- JSS [ sapply ( JSS [ , 'description' ] , Encoding ) == 'unknown' , ]
attributes ( JSS )
remove_HTML_markup <- function ( s ) 
        tryCatch ( {
                doc <- htmlTreeParse ( paste ( "<!DOCTYPE html>" , s ) , asText = TRUE , trim = FALSE )
                xmlValue ( xmlRoot ( doc ) )
        } ， error = function ( s ) s)
corpus <- Corpus ( VectorSource ( sapply ( JSS [ , 'description' ] , remove_HTML_markup ) ) )
inspect ( corpus [ 1 : 2 ] )
meta ( corpus [[ 1 ]] )
# 形成词频矩阵
JSS_dtm <- DocumentTermMatrix ( corpus , control = list ( stemming = TRUE , stopwords = TRUE , minWordLength=3 , 
                                                          removeNumbers = TRUE , removePunctuation = TRUE , 
                                                          weighting = weightTf ) )

JSS_dtm
# 整体信息描述
dim ( JSS_dtm )
class ( JSS_dtm )
summary ( col_sums ( JSS_dtm ) )
names ( which.max ( col_sums ( JSS_dtm ) ) )
findFreqTerms ( JSS_dtm , 50 )
Freq <- col_sums ( JSS_dtm )
word <- names ( Freq )
wordcloud ( word , Freq , min.freq = 30 , scale = c ( 3 , 0.4 ) , 
            random.order = F , colors = 'red' ,  rot.per = 0.1 )
# 特征选择
term_tfidf <- tapply ( JSS_dtm $ v / row_sums ( JSS_dtm ) [ JSS_dtm $ i ] , JSS_dtm $ j , mean ) *
        log2 ( nDocs ( JSS_dtm ) / col_sums ( JSS_dtm > 0 ) )
summary ( term_tfidf )
JSS_dtm <- JSS_dtm [ , term_tfidf >= 0.1 ]
JSS_dtm <- JSS_dtm [ row_sums ( JSS_dtm ) > 0 , ]
summary ( col_sums ( JSS_dtm ) )
dim ( JSS_dtm )
k<-30
SEED<-2010
jss_LDA <- list ( VEM = LDA ( JSS_dtm , k = k , control = list ( seed = SEED ) ) ,
                  VEM_fixed = LDA (JSS_dtm , k = k ,
                                   Control = list ( estimate.alpha = FALSE , seed = SEED ) ) , 
                  Gibbs = LDA (JSS_dtm , k = k , method = 'Gibbs' , control = list ( seed = SEED ) ) )
Terms <- terms ( jss_LDA [[ 'VEM' ]] , 10 )
Terms [ , 1 : 5 ]
Topic <- topics ( jss_LDA [[ "VEM" ]] )
table ( Topic ) # 1-30为相应主题，第二行为相应次数
# 文本聚类
DTM.clust <- hclust ( dist ( JSS_dtm [ Topic == 1 , ] ) )
plot ( DTM.clust )

# 14.3.2拓展案例：房地产网络舆情分析
# 加载R包并读入数据
library ( tm )
library ( tmcn )
library ( slam )
library ( Rwordseg )
library ( foreach )
library ( wordcloud )
EIN <- read.csv ( '搜房房地产即时资讯.csv' , header = T , stringsAsFactors = F ) [ 1 : 100 , ]
EIN.matter <- EIN $ 内容
attributes ( EIN )
EIN.matter <- gsub ( pattern = "[a-zA-Z\\/\\.0-9]" , "" , EIN.matter ) # 提出数字和字母
# 将原来的文档按照标点分开，每个小句或半句话形成新文档
EIN.matter <- gsub ( pattern = '[，。；、：]' , '#' , EIN.matter)
MA <- foreach ( j = 1 : length ( EIN.matter ) , combine = 'c' ) % do % {
        unlist ( strsplit ( as.character ( EIN.matter [ j ] ) , split = '#' ) )
}
# 构造语料库结构
corpus <- Corpus ( VectorSource ( MA ) )
# 分词
# 读取房地产市场专有词汇
doc1 <- unlist ( read.csv ( '房地产专有词汇.csv' , header = F , stringsAsFactors = F ) )
# 读取房地产新闻所用倾向性词汇
doc2 <- read.csv ( 'word.csv' , header = T , stringsAsFactors = F)
doc <- c ( doc1 , doc2 [ nchar ( doc2 [ , 1 ] ) >= 1 , 1 ] , doc2 [ nchar ( doc2 [ , 2 ] ) >= 1 , 2 ] )
doc <- union ( NULL , doc )
# 将词汇插入词典
insertWords ( doc )
# 分词，在分词中需将分词结果segmentCN(x)与空格相互连接，以保证词频矩阵的准确性
d.corpus <- tm_map ( corpus , content_transformer ( function ( x )
        { paste ( segmentCN ( x ) , ' ' , collapse = '' ) } ) )
# 除去中文停用词
d.corpus <- tm_map ( d.corpus , content_transformer ( removeWords ) , stopwordsCN ( ) )
d.corpus <- Corpus ( VectorSource ( d.corpus ) )
# 构造TermDocumentMatrix 矩阵
control = list ( wordLengths = c ( 2 , Inf ) ,
                 removeNumbers = TRUE , 
                 removePunctuation = list ( preserve_intra_word_dashes = F )
                 )
dtm <- DocumentTermMatrix ( d.corpus , control = control )
colnames ( dtm ) <- gsub (' ' , ' ' , colnames ( dtm ) ) #除去词汇后多余空格
Freq <- col_sums ( dtm )
word <- names ( Freq )
wordcloud ( word , Freq , min.freq = 30 , scale = c ( 3 , 0.3 ) , 
            random.order = F , colors = 'red' ,  rot.per = 0.1 )
TW <- findFreqTerms ( dtm , 100 )
TW <- TW [ nchar ( TW ) >= 2 ]
TW
findAssocs ( dtm , "公积金" , 0.2 )

