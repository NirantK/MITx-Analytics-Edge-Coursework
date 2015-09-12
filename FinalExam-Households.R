Households = read.csv("Households.csv")
library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
k = 10
KMC = kmeans(HouseholdsNorm, centers = k, iter.max = 1000)
clusters = KMC$cluster
kc1 = subset(Households, clusters == 1)
kc2 = subset(Households, clusters == 2)
kc3 = subset(Households, clusters == 3)
kc4 = subset(Households, clusters == 4)
kc5 = subset(Households, clusters == 5)
kc6 = subset(Households, clusters == 6)
kc7 = subset(Households, clusters == 7)
kc8 = subset(Households, clusters == 8)
kc9 = subset(Households, clusters == 9)
kc10 = subset(Households, clusters == 10)


set.seed(5000)
k = 5
KMC = kmeans(HouseholdsNorm, centers = k, iter.max = 1000)
clusters = KMC$cluster
kc1 = subset(Households, clusters == 1)
kc2 = subset(Households, clusters == 2)
kc3 = subset(Households, clusters == 3)
kc4 = subset(Households, clusters == 4)
kc5 = subset(Households, clusters == 5)
?hclust
ClusterShoppers2 <- hclust(distances, method = "ward.D")

