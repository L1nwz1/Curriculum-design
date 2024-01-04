data <- read.csv("D:\\因子分析数据.csv",row=1)
library("psych")

#样本相关矩阵
R<-cor(data)
R
# 因子分析
pca_result <- pca(data, nfactors = 4, rotate = "none")

# 旋转前的方差贡献率
pca_result$Vaccounted

# 计算旋转前的因子载荷矩阵
factor_loadings_before_rotation <- pca_result$loadings

# 因子旋转
rotated_result <- fa(data, nfactors = 4, rotate = "varimax")  # 使用varimax方法进行因子旋转

# 计算旋转后的方差贡献率
rotated_result$Vaccounted

# 计算旋转后的因子载荷矩阵
factor_loadings_after_rotation <- rotated_result$loadings

factor_loadings_after_rotation

# 打印结果
print("旋转前的因子载荷矩阵：")
print(factor_loadings_before_rotation)

print("旋转后的因子载荷矩阵：")
print(factor_loadings_after_rotation)

# 计算综合因子得分
rotated_result[["scores"]]
a<- rotated_result[["Vaccounted"]][2,1:4]
a1<- t(matrix(rep(a, times=28),nrow=28, ncol=4,byrow=TRUE))
b<- rotated_result[["scores"]]
scores<-(b %*% a1/sum(a1[1:4,1]))[,1]
rank(-scores)
