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
