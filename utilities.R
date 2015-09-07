plotLineGraph <- function(dat,polarity){
  # note:
  # 'dat' contains the dates for the tweets while 'polarity' conatins the corresponding polarity
  # Following is one of the ways to plot the hourly polarity among many other possible ways. One way is to used date object.
  newDf = cbind(dat, polarity)
  newDf <- as.data.frame(newDf)
  
  newDf_postive = newDf[which(newDf$polarity == 'positive'),]
  newDf_negative = newDf[which(newDf$polarity == 'negative'),]
  
  newDf_postive = mapply(function(x) substr(x,1,13) ,newDf_postive$dat)
  newDf_postive = as.data.frame(newDf_postive)
  
  newDf_negative = mapply(function(x) substr(x,1,13) ,newDf_negative$dat)
  newDf_negative = as.data.frame(newDf_negative)
  
  colnames(newDf_negative) <- colnames(newDf_postive) <- c("date")
  x_axis_ticks = unique(rbind(newDf_postive,newDf_negative))
  x_axis_ticks = as.data.frame(substr(x_axis_ticks$date,9,13) , stringsAsFactors=FALSE )
  
  PostivetweeetCountPerHour = aggregate(newDf_postive$date, list(newDf_postive$date), FUN=length)
  NegativetweeetCountPerHour = aggregate(newDf_negative$date, list(newDf_negative$date), FUN=length)
  
  plot(PostivetweeetCountPerHour$x,  ylim=c(0,700), typ='l', ann=F, xaxt="n", las=2)
  box()
  lines(NegativetweeetCountPerHour$x, type="l", col="red")

  title(main="tweets with 'Bihar', sentiment analysis", col.main="blue", font.main=2)
  title(xlab="Time (Hour)", col.lab=rgb(0.1,0.1,0.1))
  title(ylab="No of tweets with a particular sentiment", col.lab=rgb(0.1,0.1,0.1))
  axis(side = 1, at = x_axis_ticks$x_axis_ticks)
  g_range <- range(0, PostivetweeetCountPerHour$x, NegativetweeetCountPerHour$x)
  legend(1, g_range[2], c("postive","negative"), cex=0.8, col=c("black","red"),  lty=1:1)
}

getEmotions <- function(dat){
  tag_class_emo = classify_emotion(dat$text, algorithm="bayes", prior=1.0)
  #tag_class_emo = classify_emotion(dat$text, algorithm="bayes", prior=1.0, verbose=TRUE)
  
  emotion = tag_class_emo[,7]
  emotion[is.na(emotion)] = "unknown"

  sentiment_dataframe = data.frame(text=dat$text, emotion=emotion, stringsAsFactors=FALSE)
  sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  print(ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle('Sentiment Analysis of Tweets on Twitter about the tag') +
    theme(legend.position='right') + ylab('# Tweets') + xlab('Emotions'))
  
  return(emotion)
}

getSentiments <- function(dat){
  tag_class_pol = classify_polarity(dat$text, algorithm="bayes")
  polarity = tag_class_pol[,4]
  
  sentiment_dataframe = data.frame(text=dat$text, polarity=polarity, stringsAsFactors=FALSE)
  sentiment_dataframe = within(sentiment_dataframe, polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))
  
  print(ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle('Sentiment Analysis of Tweets on Twitter about tag') +
    theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories'))
  
  return(polarity)
}
