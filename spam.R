emails = read.csv("emails.csv", stringsAsFactors = F)
library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
emailsSparse <- as.data.frame(as.matrix(spdtm))  # make.names is set to true to make the variable names of emailsSparse valid
colnames(emailsSparse) <- make.names(colnames(emailsSparse), unique = T)
emailsSparse$spam <- emails$spam

# Create a data set where spam == 0 (ham). The ham dataset is certainly
# personalized to Vincent Kaminski, and therefore it might not generalize
# well to a general email user. Caution is definitely necessary before
# applying the filters derived in this problem to other email users.
ham <- emailsSparse[emailsSparse$spam == 0, ]
# How many word stems appear at least 5000 times in the ham emails in the
# dataset?
sum(colSums(ham) >= 5000)
sum(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split ==T)
test = subset(emailsSparse, split ==F)
spamLog <- glm(spam ~ ., data = train, family = binomial)
predLog <- predict(spamLog, type = "response")
sum(predLog < 1e-05)


library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = train, method = "class")
prp(spamCART)

set.seed(123)
library(randomForest)
spamRF = randomForest(spam ~ ., data = train, method = "class")

predRF <- predict(spamRF, type = "prob")[, 2]
tRF <- table(train$spam, predRF >= 0.5)
library(ROCR)
predROCRRF = prediction(predRF, train$spam)
performance(predROCRRF, "auc")@y.values