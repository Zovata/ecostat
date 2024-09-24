# 加载包和数据----
data("iris")
head(iris)
library(vegan)
library(ggplot2)
library(tidyverse)
library(cowplot)
iris1 <- iris %>% select(!Species)
head(iris1)
# 先用pca降维----
# 对iris数据集进行PCA分析
pca_result <- prcomp(iris[, -5], scale. = TRUE) # 排除种类列，并对数据标准化

# 查看主成分
summary(pca_result)

# 将PCA结果转换为数据框，以便绘图
pca_df <- data.frame(pca_result$x[, 1:2], Species = iris$Species)

# 使用ggplot2进行绘图
pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point() + # 添加散点图
  labs(x = "Principal Component 1", y = "Principal Component 2", 
       title = "PCA of Iris Data") + # 设置坐标轴标签和图表标题
  theme_bw() +
  theme(legend.position = "right") # 设置图例位置

# 聚类----
# 设置随机种子以确保结果可重现
set.seed(123)

# 使用PCA结果的前两个主成分进行K-means聚类
kmeans_result <- kmeans(pca_df[, 1:2], centers = 3, nstart = 10)

# 将聚类结果添加到原始数据框中
pca_df$cluster <- as.factor(kmeans_result$cluster)

# 使用ggplot2绘制聚类结果
cluster <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  geom_point(data = as.data.frame(kmeans_result$centers), # 绘制质心
             aes(x = PC1, y = PC2), color = "yellow", size = 4) +
  labs(x = "Principal Component 1", y = "Principal Component 2",
       title = "K-means Clustering on PCA Results") +
  theme_bw()

p <- plot_grid(pca, cluster, ncol = 2)  # 两列排列
p
ggsave("p.png", p, width = 10, height = 8, dpi = 300)

