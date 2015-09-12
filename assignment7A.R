# Election Forecasting
library(ggplot2)
library(ggmap)
library(maps)

statesMap = map_data("state")
# how many groups are there?
length(unique(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# polling data and prediction
polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year<2012)
Test = subset(polling, Year==2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
# plot on map : Republican or not?
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# the decorated map
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
