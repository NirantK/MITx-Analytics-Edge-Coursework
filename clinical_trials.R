trials <- read.csv("clinical_trial.csv", stringsAsFactors = F)
str(trials)

# Pre process data
library(tm)
# Create Corpus
corpusTitle <- Corpus(VectorSource(trials$title)) 
corpusAbstract <- Corpus(VectorSource(trials$abstract)) 

# Convert to lower case
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

#Convert to text document
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract  = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation 
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove Stop words
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Look at the first document
corpusTitle[[1]]

# Create matrix
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

#Retain only top 5% words
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

#building a df
abstractDf = as.data.frame(as.matrix(dtmAbstract))
titleDf = as.data.frame(as.matrix(dtmTitle))

# We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions. However, some of the variables in these data frames have the same names.

colnames(titleDf) <- paste0("T", colnames(titleDf))
colnames(abstractDf) <- paste0("A", colnames(abstractDf))
colnames(titleDf)
colnames(abstractDf)

# Combine the two dataframes
dtm <- cbind(titleDf, abstractDf)
# Add the trial variable
dtm$trial <- trials$trial

# Load CaTools
library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, spl == T)
test <- subset(dtm, spl == F)

# baseline model accuracy on the training set
table(train$trial)[1] / sum(table(train$trial))
library(rpart)
library(rpart.plot)

nkCART = rpart(trial ~. ,data = train,  method = "class")
prp(nkCART)

predTrain <- predict(nkCART)[,2]
# Accuracy on the training set
t1 <- table(train$trial, predTrain >= 0.5)



trialsCART= nkCART
predTest <- predict(trialsCART, newdata = test)[,2]
t2 <- table(test$trial, predTest >= 0.5)
(t2[1,1] + t2[2,2])/(sum(t2))

library(ROCR)
predROCR = prediction(predTest, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
auc = performance(predROCR, "auc")
