# library
install.packages("NbClust")
library(tidyverse)
library(ramify)
library(h2o)
library(ggpubr)
library(GGally)
library(factoextra)
library(RColorBrewer)
library(ggplotify)
library(hrbrthemes)
library(dendextend)
library(plyr)
library(dplyr)
library(corrplot)
library(ClusterR)
library(knitr)
library(gridExtra)
library(magrittr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(gridExtra)
library(grid)
library(lattice)
library(GGally)
library(factoextra)
library(ClustOfVar)
library(cluster)
library(KSD)
library(mclust)
library(gridExtra)
library(fpc)
library(dbscan)
library(plotly)
library(NbClust)
library(pracma)
library(cluster)
install.packages('subspace')
library(subspace)
library(bootnet)
library(latticeExtra)
library(igraph)
library(clValid)
library(fossil)
# --------------------------------------------------------------------------------------------
#dataset
#讀取原始資料
Original_Data <- read.csv("C:/Users/eva/Desktop/作業 上課資料(清大)/大四下/多變量/final/Mall_Customers.csv",header = TRUE)
colnames(Original_Data)<-c("CustomerID","Genre","age", "annual_income", "spending_score")
head(Original_Data)
Original_Data[,5]<-as.numeric(Original_Data[,5])
#去除無數值意義之資料
User_Data <- Original_Data[,3:5] 
#標準化資料
Standardize_Data <- scale(User_Data, center = TRUE, scale = TRUE)
data_m<-Original_Data[,4:5]
#---------------------------------------------------------------------------------------------
# EDA
# Scatterplot
qplot(annual_income, spending_score,
      colour = factor(Genre), data = Original_Data)

#Histogram
graph<-gather(User_Data, Attributes, happen_times, 1:3 )
ggplot(graph, aes(x=happen_times, fill=Attributes))+
  geom_histogram(colour="black", show.legend=FALSE)+
  facet_wrap(~Attributes, scales="free_x") + #以attribute變量進行數據分類, free_x代表自由調整x軸刻度範圍
  labs(x="happen_times", y="Frequency",
       title="Iris Attributes - Histograms") +
  theme_bw()

# --------------------------------------------------------------------------------------------
# PCA
data_pca<-prcomp(User_Data, scale. = T)
summary(data_pca)
screeplot(data_pca, type="lines")
#畫累積變異
vars <- (data_pca$sdev)^2
props <- vars / sum(vars) 
cumulative_props <- cumsum(props)
plot(cumulative_props)
var <- get_pca_var(data_pca)
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(data_pca, choice = "var", axes = 1:2)
fviz_pca_var(data_pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
#繪製散佈圖矩陣
ggpairs(as.data.frame(Standardize_Data), title="correlogram") 

#繪製3d散佈圖
plot_ly(as.data.frame(data_pca$x[, 1:3]), x = ~PC1, y = ~PC2, z = ~PC3, marker = list(color = "darkgoldenrod", size=1.5))

pca_data_cl_4 <- kmeans(data_pca$x[, 1:3], 4)
pca_data_cl_5 <- kmeans(data_pca$x[, 1:3], 5)
pca_data_cl_6 <- kmeans(data_pca$x[, 1:3], 6)
plot_ly(as.data.frame(data_pca$x[, 1:3]), x = ~PC1, y = ~PC2, z = ~PC3, marker = list(color = ~pca_data_cl_4$cluster, size=2.5))
plot_ly(as.data.frame(data_pca$x[, 1:3]), x = ~PC1, y = ~PC2, z = ~PC3, marker = list(color = ~pca_data_cl_5$cluster, size=2.5))
plot_ly(as.data.frame(data_pca$x[, 1:3]), x = ~PC1, y = ~PC2, z = ~PC3, marker = list(color = ~pca_data_cl_6$cluster, size=2.5))


#------------------------------------------------------------------------------------------
# Hierarchical Clustering

# Distance matrix
E.dist <- dist(data_m, method="euclidean") # 歐式距離
M.dist <- dist(data_m, method="manhattan") # 曼哈頓距離

# Dendrogram
par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化

# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")

# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")

# Different linking method
hc<-par(mfrow= c(2,3))
plot(hclust(E.dist, method="single"),xlab = "single-linkage", main='single-linkage method', hang = -1, cex = 0.6, labels = F)   # 最近法
plot(hclust(E.dist, method="complete"), xlab = "complete-linkage", main='complete-linkage', hang = -1, cex = 0.6, labels = F)  # 最遠法
plot(hclust(E.dist, method="average"), xlab = "average-linkage", main='average-linkage', hang = -1, cex = 0.6, labels = F)  # 平均法
plot(hclust(E.dist, method="centroid"), xlab = "centroid-linkage", main='centroid-linkage', hang = -1, cex = 0.6, labels = F) # 中心法
plot(hclust(E.dist, method="ward.D2"), xlab = "Ward's Method", main="Ward's Method", hang = -1, cex = 0.6, labels = F)  # 華德法
par(hc)
#採用歐式距離搭配不同聚合演算法，並算出聚合係數(agglomerative coefficient)
#衡量群聚結構被辨識的程度，聚合係數越接近1代表有堅固的群聚結構(strong clustering structure)。
#在這個使用歐式距離搭配華德連結演算法的群聚係數有高達99%的表現。
m <- c( "average", "single", "complete", "ward")
m_ac=NULL
for(i in 1:length(m)){
  x<-agnes(E.dist, method=m[i])$ac
  m_ac<-append(m_ac,x)
}
names(m_ac) <- c( "average", "single", "complete", "ward")
m_ac

par(mfrow=c(1,1))
hc3 <- hclust(E.dist, method="ward.D2")
plot(hc3, hang = -1, cex = 0.6)
rect.hclust(tree =hc3, k = 2, border = "blue")
rect.hclust(tree =hc3, k = 4, border = "purple")
rect.hclust(tree =hc3, k = 5, border = "pink")
cut.h.cluster <- cutree(tree = hc3, k = 2)
table(cut.h.cluster, Original_Data$Genre)
#----------------------------------------------------------------------------------------
# K-Means
# Normalization
dataNorm <- as.data.frame(scale(data_m))

# Run the algorithm for different values of k (choose best k)

# method 1
fviz_nbclust(dataNorm, kmeans, method = "wss")
# method 2
fviz_nbclust(dataNorm, kmeans, method = "silhouette")
# method 3
fviz_gap_stat(clusGap(x = Standardize_Data,FUNcluster = kmeans, nstart = 25, K.max = 10, B = 50),maxSE = list(method = "globalmax", SE.factor = 1))


# Doing K-means with K=2
set.seed(1234)
# num of cluster=3
data_km2 <- kmeans(dataNorm, centers=2)
# Mean values of each cluster
aggregate(data_m, by=list(data_km2$cluster), mean)

# clustering
ggpairs(cbind(data_m, Cluster=as.factor(data_km2$cluster)),
        columns=1:2, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous = gglegend("points")),
        axisLabels="none", switch="both")+ 
  theme_bw()


# comparison
table(data_km2$cluster, Original_Data$Genre)


data_km5 <- kmeans(dataNorm, centers=5)
data_km3 <- kmeans(dataNorm, centers=3)
p2<-fviz_cluster(data_km2, data = dataNorm)+
  ggtitle("k = 2")
p3<-fviz_cluster(data_km3, data = dataNorm)+
  ggtitle("k = 3")
p5<-fviz_cluster(data_km5, data = dataNorm)+
  ggtitle("k = 5")

grid.arrange(p2,p3,p5, nrow = 1)

# --------------------------------------------------------------------------------------------
# GMM
mb = Mclust(data_m)
# optimal selected model
mb$modelName

# optimal number of cluster
mb$G

# probality for an observation to be in a given cluster
head(mb$z)

# get probabilities, means, variances
summary(mb, parameters = TRUE)

#Comparison
table(Original_Data$Genre, mb$classification)
plot(mb, what=c("classification"))
# ---------------------------------------------------------------------------------------------

# DBSCAN
db = fpc::dbscan(data_m, eps = 0.15, MinPts = 2)
plot(db, data_m, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, data_m, stand = FALSE, frame = FALSE, geom = "point")


# -------------------------------------------------------------------------------------------
#Clique(理論上最合適)
library(meanShiftR)
aa <- data.frame(scale(data_m$annual_income),scale(data_m$spending_score))
result <- meanShift(
  as.matrix(aa),                      
  trainData = as.matrix(aa),                       
  algorithm = "KDTREE",
  kernelType = "BIWEIGHT",
  alpha = 0.0,
  iterations=1000
)
result

#assignment
meanShiftR_kd_assignment <- result$assignment
table(result$assignment)

# value
meanShiftR_kd_value <- result$value
library(ggplot2)
plot(result$value, col=factor(meanShiftR_kd_assignment))

# ---------------------------------------------------------------------------------------------
# Clarans

suppressMessages(suppressWarnings(library(cluster)))
results <- clara(data_m, 5, metric = "euclidean", samples = 2, pamLike = F)
newmall <- data_m
newmall$cluster <- results$clustering

A <- xyplot(spending_score ~ annual_income, group = cluster, data = newmall, auto.key = 
              list(space = "right"), par.settings = list(superpose.symbol = list(pch = 0, cex 
                                                                                 = 2, col = c("green", "black", "red", "blue"))))
C <- xyplot(results$medoids[,c("spending_score")] ~ results$medoids[,c("annual_income")], pch = "@", cex 
            = 1, col = c("green", "black", "red", "blue"), auto.key = 
              list(space = "right"))
D <- A + as.layer(C)
D
# By setting pamLike = FALSE and samples = 1 one can get CLARA algorithm clustering, 
# while by setting pamLike = FALSE and samples = n with n > 1 one can get CLARANS algorithm clustering.

# Clustering tendency
gradient_col = list(low = "steelblue", high = "white")
get_clust_tendency(User_Data, n = 50, gradient = gradient_col)

#DUNN
## hierarchical clustering Dunn
dunn(E.dist, cut.h.cluster)

## K-means clustering Dunn
dunn(E.dist, data_km2$cluster)

## GMM clustering Dunn
dunn(E.dist, mb$classification)

## DBSCAN clustering Dunn
dunn(E.dist, db$cluster)

## CLIQUE clustering Dunn
dunn(E.dist, meanShiftR_kd_assignment)

## CLARANS clustering Dunn
dunn(E.dist, newmall$cluster)

#Rand.index
## hierarchical clustering Rand.index
rand.index(as.numeric(iris$Species), cut.h.cluster)

## K-means clustering Rand.index
rand.index(as.numeric(iris$Species), data_km2$cluster)

## GMM clustering Rand.index
rand.index(as.numeric(iris$Species), mb$classification)

## DBSCAN clustering Rand.index
rand.index(as.numeric(iris$Species), db$cluster)

## CLIQUE clustering Rand.index
rand.index(as.numeric(iris$Species), meanShiftR_kd_assignment)

## CLARANS clustering Rand.index
rand.index(as.numeric(iris$Species), newmall$cluster)


# Silhouette coefficient of observations
##Hierarchical
sil <- silhouette(cut.h.cluster, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  cut.h.cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths

##K-means
sil <- silhouette(data_km3$cluster, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  data_km3$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths

##GMM
sil <- silhouette(mb$classification, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  mb$classification)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths

##DBSCAN
sil <- silhouette(db$cluster, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  db$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths

##CLIQUE
sil <- silhouette(meanShiftR_kd_assignment, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  meanShiftR_kd_assignment)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths

##CLARANS
sil <- silhouette(newmall$cluster, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  newmall$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths


























































































