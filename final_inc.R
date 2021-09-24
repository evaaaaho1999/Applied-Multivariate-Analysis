# library
install.packages("subspace")
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
library(subspace)
library(bootnet)
library(latticeExtra)
library(igraph)
library(readxl)
library(misty)
library(clValid)
library(fossil)
# --------------------------------------------------------------------------------------------
#dataset
data <- read.sav("C:/Users/eva/Desktop/作業 上課資料(清大)/大四下/多變量/final/inc108.sav")
head(data)
data_h<-cbind(data$c6a, data$c6b, data$itm1110, data$itm1080, data$itm1150, data$itm400, data$itm330)
colnames(data_h)<-c("Land_occupation", "Construction_site", "traffic", "medical", "leisure", "income", "property")
head(data_h)
data_h<-na.omit(data_h)
data_h<-as.data.frame(data_h)
#---------------------------------------------------------------------------------------------
# PCA
data_pca<-prcomp(data_h, scale. = T)
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

PCA_tr<-data_pca$x[,1:3]

# --------------------------------------------------------------------------------------------
# EDA
# Scatterplot
plot(data_h$income, data_h$Land_occupation)

#Histogram
graph<-gather(data_h, Attributes, happen_times, 1:7 )
ggplot(graph, aes(x=happen_times, fill=Attributes))+
  geom_histogram(colour="black", show.legend=FALSE)+
  facet_wrap(~Attributes, scales="free_x") + #以attribute變量進行數據分類, free_x代表自由調整x軸刻度範圍
  labs(x="happen_times", y="Frequency",
       title="Iris Attributes - Histograms") +
  theme_bw()



#------------------------------------------------------------------------------------------
# # Hierarchical Clustering
# 
# # Distance matrix
# E.dist <- dist(PCA_tr, method="euclidean") # 歐式距離
# M.dist <- dist(PCA_tr, method="manhattan") # 曼哈頓距離
# 
# # Dendrogram
# par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化
# 
# # 使用歐式距離進行分群
# h.E.cluster <- hclust(E.dist)
# plot(h.E.cluster, xlab="歐式距離")
# 
# # 使用曼哈頓距離進行分群
# h.M.cluster <- hclust(M.dist) 
# plot(h.M.cluster, xlab="曼哈頓距離")
# 
# # Different linking method
# hc<-par(mfrow= c(2,3))
# plot(hclust(E.dist, method="single"),xlab = "single-linkage", main='single-linkage method', hang = -1, cex = 0.6, labels = F)   # 最近法
# plot(hclust(E.dist, method="complete"), xlab = "complete-linkage", main='complete-linkage', hang = -1, cex = 0.6, labels = F)  # 最遠法
# plot(hclust(E.dist, method="average"), xlab = "average-linkage", main='average-linkage', hang = -1, cex = 0.6, labels = F)  # 平均法
# plot(hclust(E.dist, method="centroid"), xlab = "centroid-linkage", main='centroid-linkage', hang = -1, cex = 0.6, labels = F) # 中心法
# plot(hclust(E.dist, method="ward.D2"), xlab = "Ward's Method", main="Ward's Method", hang = -1, cex = 0.6, labels = F)  # 華德法
# par(hc)
# #採用歐式距離搭配不同聚合演算法，並算出聚合係數(agglomerative coefficient)
# #衡量群聚結構被辨識的程度，聚合係數越接近1代表有堅固的群聚結構(strong clustering structure)。
# #在這個使用歐式距離搭配華德連結演算法的群聚係數有高達99%的表現。
# m <- c( "average", "single", "complete", "ward")
# m_ac=NULL
# for(i in 1:length(m)){
#   x<-agnes(E.dist, method=m[i])$ac
#   m_ac<-append(m_ac,x)
# }
# names(m_ac) <- c( "average", "single", "complete", "ward")
# m_ac
# 
# par(mfrow=c(1,1))
# hc3 <- hclust(E.dist, method="ward.D2")
# plot(hc3, hang = -1, cex = 0.6)
# rect.hclust(tree =hc3, k = 2, border = "blue")
# rect.hclust(tree =hc3, k = 3, border = "purple")
# rect.hclust(tree =hc3, k = 4, border = "pink")
# cut.h.cluster <- cutree(tree = hc3, k = 4)
# 
# #----------------------------------------------------------------------------------------
# # K-Means
# # Normalization
# dataNorm <- as.data.frame(scale(PCA_tr))
# 
# # Run the algorithm for different values of k (choose best k)
# 
# # method 1
# fviz_nbclust(dataNorm, kmeans, method = "wss")
# # method 2
# fviz_nbclust(dataNorm, kmeans, method = "silhouette")
# # method 3
# fviz_gap_stat(clusGap(x = data_h,FUNcluster = kmeans, nstart = 25, K.max = 10, B = 50),maxSE = list(method = "globalmax", SE.factor = 1))
# 
# 
# # Doing K-means with K=2
# set.seed(1234)
# # num of cluster=4
# data_km4 <- kmeans(dataNorm, centers=4)
# # Mean values of each cluster
# aggregate(PCA_tr, by=list(data_km4$cluster), mean)
# 
# # clustering
# ggpairs(cbind(data_h, Cluster=as.factor(data_km4$cluster)),
#         columns=1:2, aes(colour=Cluster, alpha=0.5),
#         lower=list(continuous="points"),
#         upper=list(continuous = gglegend("points")),
#         axisLabels="none", switch="both")+ 
#   theme_bw()
# 
# 
# 
# 
# 
# 
# data_km7 <- kmeans(dataNorm, centers=7)
# p4<-fviz_cluster(data_km4, data = dataNorm)+
#   ggtitle("k = 4")
# p7<-fviz_cluster(data_km7, data = dataNorm)+
#   ggtitle("k = 7")
# 
# grid.arrange(p4,p7, nrow = 1)

# --------------------------------------------------------------------------------------------
# GMM
mb = Mclust(PCA_tr)
# optimal selected model
mb$modelName

# optimal number of cluster
mb$G

# probality for an observation to be in a given cluster
head(mb$z)

# get probabilities, means, variances
summary(mb, parameters = TRUE)

#Comparison
par(mfrow=c(1,1))
plot(mb, what=c("classification"))
# ---------------------------------------------------------------------------------------------

# # DBSCAN
# db = fpc::dbscan(PCA_tr, eps = 0.15, MinPts = 5)
# plot(db, PCA_tr, main = "DBSCAN", frame = FALSE)
# fviz_cluster(db, PCA_tr, stand = FALSE, frame = FALSE, geom = "point")
# 
# 
# # -------------------------------------------------------------------------------------------
# #Clique
# library(meanShiftR)
# aa <- data.frame(scale(PCA_tr[,1]),scale(PCA_tr[,2]))
# result <- meanShift(
#   as.matrix(aa),                      
#   trainData = as.matrix(aa),                       
#   algorithm = "KDTREE",
#   kernelType = "BIWEIGHT",
#   alpha = 0.0,
#   iterations=1000
# )
# result
# 
# #assignment
# meanShiftR_kd_assignment <- result$assignment
# table(result$assignment)
# 
# # value
# meanShiftR_kd_value <- result$value
# library(ggplot2)
# plot(result$value, col=factor(meanShiftR_kd_assignment))

# ---------------------------------------------------------------------------------------------
# Clarans

suppressMessages(suppressWarnings(library(cluster)))
results <- clara(PCA_tr, 4, metric = "euclidean", samples = 2, pamLike = F)
newinc <- as.data.frame(PCA_tr)
newinc$cluster <- results$clustering

A <- xyplot(PC1 ~ PC2+PC3, group = cluster, data = newinc, auto.key = 
              list(space = "right"), par.settings = list(superpose.symbol = list(pch = 0, cex 
                                                                                 = 2, col = c("green", "black", "red", "blue"))))
C <- xyplot(results$medoids[,c("PC1")] ~ results$medoids[,c("PC2")]+results$medoids[,c("PC3")], pch = "@", cex 
            = 1, col = c("green", "black", "red", "blue"), auto.key = 
              list(space = "right"))
D <- A + as.layer(C)
D
# By setting pamLike = FALSE and samples = 1 one can get CLARA algorithm clustering, 
# while by setting pamLike = FALSE and samples = n with n > 1 one can get CLARANS algorithm clustering.
plot_ly(as.data.frame(data_pca$x[, 1:3]), x = ~PC1, y = ~PC2, z = ~PC3, marker = list(color = ~results$clustering, size=2.5))



# Clustering tendency
gradient_col = list(low = "steelblue", high = "white")
get_clust_tendency(data_h, n = 50, gradient = gradient_col)

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
dunn(E.dist, newinc$cluster)

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
rand.index(as.numeric(iris$Species),newinc$cluster)


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
sil <- silhouette(newinc$cluster, E.dist)
head(sil[, 1:3], 10)
# Silhouette plot
fviz_silhouette(sil)
si.sum <- summary(sil)
si.sum
km_stats <- cluster.stats(E.dist,  newinc$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths













































