rm(list = ls())

# For the first time, uncomment the following code to install required packages.
#install.packages('devtools')
#install.packages("MASS")
#install.packages("Rstem")
#install.packages("RTextTools")
#install.packages("MASS")
#library('devtools')
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz)"
#install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz)"
#install.packages("e1071")
#install.packages("RTextTools")

library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(Rgraphviz) # Correlation plots
library(wordcloud) # routines to create word cloud
library(rpart)  # cart models
library(RWeka)  # R wrappers for weka tools
library(ROCR)   # model performance routines
library(pROC)   # model performance routines
library('MASS')
library('devtools')
library(sentiment) # sentiments
library(twitteR) # twitter
library(plyr)
library(RColorBrewer) # color plots
library(e1071) # naive bayes
library(RTextTools) # accuracy measure

#import tweets data file
dat <- read.csv("historical_tweet_bihar_lookup_2015.csv", sep = ",", header = T,  row.names=NULL, stringsAsFactors = FALSE)

# data clean up
dat$text[grep("RT @", dat$text)] <- ""
dat$text[grep("RT: ", dat$text)] <- ""
dat$text <- gsub("@(.+? )","",paste(as.character(dat$text)," ",sep = ""))
dat$text <- gsub("http://(.+? )","",paste(as.character(dat$text)," ",sep = ""))
dat$text <- iconv(as.character(dat$text), "latin1", "ASCII", sub="")
dat$text <- gsub("^\\s+|\\s+$", "", as.character(dat$text))
dat <- dat[which(nchar(as.character(dat$text))>3),]

# a dummy tweet added just to show the text transformations.
dat$text[1] = 'a tweeT, For showing 4 to 5 different tranforms. This is dummy-data'

#create corpus
docs <- Corpus(VectorSource(dat$text), readerControl = list(language = "en"))
docs

inspect(docs[2])
as.character(docs[[2]])

#transformations
getTransformations()

as.character(docs[[1]])
docs <- tm_map(docs, content_transformer(tolower))
as.character(docs[[1]])
docs <- tm_map(docs, removeNumbers)
as.character(docs[[1]])
docs <- tm_map(docs, removePunctuation)
as.character(docs[[1]])
docs <- tm_map(docs, removeWords, stopwords("english"))
#docs <- tm_map(docs, removeWords,c("w1", "w2"))
# stopwords("english")
as.character(docs[[1]])

#stemming
docs <- tm_map(docs, stemDocument)
as.character(docs[[1]])

#Document Term Matrix
dtm <- DocumentTermMatrix(docs)
dtm

#remove sparsity
dim(dtm)

dtms <- removeSparseTerms(dtm, 0.999) # package matrix contains sparse matrix
dim(dtms)

#Explore DTM
freq <- colSums(as.matrix(dtms))
length(freq)

ord <- order(freq)
#Look at the least frequent terms
freq[head(ord)]
#Look at the max frequent terms
freq[tail(ord)]

#Get top frequent terms
findFreqTerms(dtm, lowfreq=100)
findFreqTerms(dtm, lowfreq=1000)

#Frequent Associations
findAssocs(dtm, "data", corlimit=0.6)

#Plot correlated terms
plot(dtm,terms=findFreqTerms(dtm, lowfreq=550),corThreshold=0.5, attrs = list(node = list(shape = "rectangle",fixedsize=T,fontsize=30)))

#Plot a wordcloud
wordcloud(names(freq), freq, min.freq=900)


#### Predictions with data
## We must note that for the demo, dummy labels are used.

# Converting the dtm to a matrix for using it with the training/ predicting task
mat = as.matrix(dtms)

# using 70% of the data for training our algorithm
rows_count <- floor(dim(mat)[[1]]*0.7)

# generate dummy labels, if not present
dummy_labels = sample(c("a","b"), rows_count, replace = TRUE)

# train the model
classifier = naiveBayes(mat[1:rows_count,], as.factor(dummy_labels))

# predict using the trained model/ classifier. (we use 20 new examples for)
predicted = predict(classifier, mat[(rows_count+1):(rows_count+20),])
predicted

# check the predication accuracy
recall_accuracy(dummy_labels[1:20], predicted)



#### Twitter analysis
source("utilities.R") # this file contains code for sentiment analysis

# Polarity: postive, negative and neutral
# emotions: joy, fear, scare etc (total 7)

# get and plot polarity for the tweets
polarity <- getSentiments(dat)
# get and plot emotions for the tweets
emotion <- getEmotions(dat)

# plot the hourly sentiment trends of the tweets
plotLineGraph(dat$created_at, polarity)


#### lets mine the tweets
pattern1 <- "bjp|modi|namo|narendramodi"
bjp <- dat[grep(pattern1, dat$text,ignore.case=TRUE), ]

pattern2 <- "nithish|jdu|nitishkumar|laloo"
jdu <- dat[grep(pattern2, dat$text, ignore.case=TRUE), ]


#### bjp tweets analysis
emotion <- getEmotions(bjp)
polarity <- getSentiments(bjp)
plotLineGraph(bjp$created_at, polarity)

#### jdu tweets analysis
emotion <- getEmotions(jdu)
polarity <- getSentiments(jdu)
plotLineGraph(jdu$created_at, polarity)


# hope by now you can predict which party will win this time. :)
