fedFunds = read.csv("federalFundsRate.csv", stringsAsFactors = F)
# table(fedFunds$RaisedFedFunds)
fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)
# LinearModel = glm(RaisedFedFunds~ ., data = fedFunds)
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
train = subset(fedFunds, spl == T)
test = subset(fedFunds, spl == F)
logisticModel = glm(RaisedFedFunds ~  PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                    family = "binomial",
                    data = fedFunds)


library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:50)*0.001)
tree = rpart(RaisedFedFunds ~ PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=fedFunds)
tr = train(RaisedFedFunds ~ PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
best.tree = tr$finalModel
