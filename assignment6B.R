# k means clustering
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)
library(caret)
preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

vectorAirlines = as.vector(airlinesNorm)
distance = dist(vectorAirlines, method = "euclidean")
clusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)
clusterGroups = cutree(clusterIntensity, k = 5)
cluster1 <- subset(airlines, clusters == 1)
nrow(cluster1)

balanceAvg <- tapply(airlines$Balance, clusterGroups, mean)
qualMilesAvg <- tapply(airlines$QualMiles, clusterGroups, mean)
bonusMilesAvg <- tapply(airlines$BonusMiles, clusterGroups, mean)
bonusTransAvg <- tapply(airlines$BonusTrans, clusterGroups, mean)
flightMilesAvg <- tapply(airlines$FlightMiles, clusterGroups, mean)
flightTransAvg <- tapply(airlines$FlightTrans, clusterGroups, mean)
daysAvg = tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

k = 5
set.seed(88)
KmeansCluster = kmeans(airlinesNorm, centers = k, iter.max=1000)
airlinesClusters = KmeansCluster$cluster
table(airlinesClusters) > 1000