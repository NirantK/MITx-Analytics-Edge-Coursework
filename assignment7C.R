tweets = read.csv("tweets.csv", stringsAsFactors = F)
library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
library(wordcloud)
allTweets[which.max(colSums(allTweets))] = 0
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), scale=c(2, 0.25), colors= brewer.pal(9, "Blues")[c(-1,-2,-3,-4)])


# tweets$Negative = as.factor(tweets$Avg <= -1)
# buriBaat = subset(tweets, tweets$Negative==TRUE)
# corpus = Corpus(VectorSource(buriBaat$Tweet))
# corpus = tm_map(corpus, tolower)
# 
# # IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
# corpus = tm_map(corpus, PlainTextDocument)
# corpus = tm_map(corpus, removePunctuation)
# corpus = tm_map(corpus, removeWords, stopwords("english"))
# 
# dtm = DocumentTermMatrix(corpus)
# allTweets = as.data.frame(as.matrix(dtm))
# library(wordcloud)
# allTweets[which.max(colSums(allTweets))] = 0
# wordcloud(words = colnames(allTweets), freq = colSums(allTweets), scale=c(4, 0.25))
