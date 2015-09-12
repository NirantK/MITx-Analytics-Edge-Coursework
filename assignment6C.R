stocks = read.csv("StocksCluster.csv")
summary(stocks)

library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == T)
stocksTest <- subset(stocks, spl == F)
stocksModel <- glm(PositiveDec~., data = stocksTrain, family = binomial)
stocksPred <- predict(stocksModel, type = "response")
table(stocksTrain$PositiveDec, stocksPred >= 0.5)

# accuracy on test data
stocksPred_test <- predict(stocksModel, newdata = stocksTest, type = "response")
t = table(stocksTest$PositiveDec, stocksPred_test >= 0.5)
testAccuracy = (t[1,1]+t[2,2])/sum(t)

# removing the dependent variable for clustering
limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

# preprocess and normalize
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)

set.seed(144)
km <- kmeans(normTrain, centers = 3)
str(km)
clusters = km$cluster
table(clusters)

# obtain training and test set cluster assignments
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

stockTrain1 <- subset(stocksTrain, clusterTrain == 1)
stockTrain2 <- subset(stocksTrain, clusterTrain == 2)
stockTrain3 <- subset(stocksTrain, clusterTrain == 3)

stockTest1 <- subset(stocksTest, clusterTest == 1)
stockTest2 <- subset(stocksTest, clusterTest == 2)
stockTest3 <- subset(stocksTest, clusterTest == 3)

stocksModel1 <- glm(PositiveDec ~., data = stockTrain1, family = binomial)
stocksModel2 <- glm(PositiveDec ~., data = stockTrain2, family = binomial)
stocksModel3 <- glm(PositiveDec ~., data = stockTrain3, family = binomial)

predictTest1 <- predict(stocksModel1, newdata = stockTest1, type = "response")
predictTest2 <- predict(stocksModel2, newdata = stockTest2, type = "response")
predictTest3 <- predict(stocksModel3, newdata = stockTest3, type = "response")

t = table(stockTest1$PositiveDec, predictTest1 >= 0.5)
accuracy1 = (t[1,1]+t[2,2])/sum(t)
accuracy1

t = table(stockTest2$PositiveDec, predictTest2 >= 0.5)
accuracy2 = (t[1,1]+t[2,2])/sum(t)
accuracy2

t = table(stockTest3$PositiveDec, predictTest3 >= 0.5)
accuracy3 = (t[1,1]+t[2,2])/sum(t)
accuracy3

AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)

t = table(AllOutcomes, AllPredictions >= 0.5)
accuracyOverall = (t[1,1]+t[2,2])/sum(t)
accuracyOverall
