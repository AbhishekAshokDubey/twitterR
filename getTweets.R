rm(list = ls())

# # install the necessary packages
# install.packages("twitteR")
# install.packages("base64enc")
# install.packages("httpuv")

library("twitteR")
library("base64enc")
library("httpuv")



# Need to register an app with you twitter account to get following keys
# twitter app page: https://apps.twitter.com/

consumer_key <- 'your key'
consumer_secret <- 'your key'
# access_token <- 'your key'
# access_secret <- 'your key'
# setup_twitter_oauth(consumer_key,
#                     consumer_secret,
#                     access_token,
#                     access_secret)

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    NULL,
                    NULL)


r_stats <- searchTwitter("bihar", n = 150, lang="en", since="2014-08-20")
length(r_stats)

dat <- twListToDF(r_stats)
write.csv(file='tweets.csv', x=dat)

# dat
# dat$text[1:5]

# dat <- tweets.df[with(tweets.df, order(-retweetCount)), ]
# sorted_tweets.df <- tweets.df[with(tweets.df, order(col1, col2)), ]
# sorted_tweets.df = # sorted_tweets.df[1:10,]

library(wordcloud) # routines to create word cloud  
wordcloud(dat$screenName, dat$retweetCount, min.freq=100)

require(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(dat$screenName, dat$retweetCount,min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.20, colors=pal2)
