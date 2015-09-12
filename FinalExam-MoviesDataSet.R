# ----------------------------------------------------------------------------------
# Linear Regression Model
# ----------------------------------------------------------------------------------
Movies = read.csv("Movies.csv")
MoviesTrain = subset(movies, movies$Year < 2010)
MoviesTest = subset(movies, movies$Year >= 2010)
str(train)
str(MoviesTest)
linearModel = lm(Worldwide ~ ., data = MoviesTrain[, 3:ncol(MoviesTrain)])
linearPrediction = predict(linearModel, newdata=MoviesTest)

SSE = sum((linearPrediction - MoviesTest$Worldwide)^2)
SSE
SST = sum((mean(MoviesTrain$Worldwide) - MoviesTest$Worldwide)^2)
R2 = 1 - (SSE/SST)
R2
# ----------------------------------------------------------------------------------
# CART
# ----------------------------------------------------------------------------------
Movies$Performance = factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent", ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average", "Poor")))
Movies$Worldwide =NULL
set.seed(15071)
sample = sample.split(Movies$Performance, SplitRatio = 0.7)
MoviesTrain = subset(Movies, sample == TRUE)
MoviesTest = subset(Movies, sample == FALSE)
library(caTools)

treeModel = rpart(Performance ~ ., data = MoviesTrain[, 3:ncol(MoviesTrain)])
rpart.plot(treeModel)
PredictTest = predict(treeModel, newdata = MoviesTest, type = "class")
table(MoviesTest$Performance, PredictTest)

PredictTrain = predict(treeModel, data = MoviesTrain, type = "class")
table(MoviesTrain$Performance, PredictTrain)

