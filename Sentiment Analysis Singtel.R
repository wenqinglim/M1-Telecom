library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

setwd("~/NUS/20162017 Semester 1/BT2101/Project")
singtel.1<-read.csv("twitter_singtel.csv", header=T)
singtel.2<-read.csv("twitter_singtelw2.csv", header=T)
singtel.3<-read.csv("twitter_singtelw3.csv", header=T)
singtel<- merge(singtel.1, singtel.2, all=T)
singtel<-merge(singtel, singtel.3, all=T)

write.csv(singtel, "twitter_singtelALL.csv")
singtel.txt<-read.csv("twitter_singtelALL.csv")
#sgforums
setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
singtel.txt<-read.csv("sgforum_singtel.csv", header=T)
singtel.txt$X_input=NULL
singtel.txt<-na.omit(singtel.txt)

#twitter
singtel.txt = singtel.txt$text
#sgforums
singtel.txt=singtel.txt$forum_post

#remove @tags, urls, and emojis
singtel.txt <-gsub("@\\w+ *", "", singtel.txt)
singtel.txt <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", singtel.txt)
singtel.txt <- iconv(singtel.txt, 'UTF-8', 'ASCII')

#SGForum Remove originally posted by tag
singtel.txt <- gsub("Originally posted by", "", singtel.txt)
singtel.txt<-gsub("singtel", "", singtel.txt)

#remove stopwords
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
singtel.txt <- rm_words(singtel.txt, tm::stopwords("en"))

#taken from: http://jeffreybreen.wordpress.com/page/2/
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    sentence = gsub('singtel', '', sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


poswords <- scan("positive-words.txt", what="character", comment.char=";")
negwords <- scan("negative-words.txt", what="character", comment.char=";")

test <- c("i believe in hate at first sight !", "cool pics^)I love it", "I HATE IT THAT I LOVE YOU")

test1<-score.sentiment(test, poswords, negwords)

singtel.sentiment<-score.sentiment(singtel.txt, poswords, negwords)
singtel.sentiment[singtel.sentiment=="NA"]<- NA
singtel.sentiment<-na.omit(singtel.sentiment)

neutral <- length(which(singtel.sentiment$score == 0))
positive <- length(which(singtel.sentiment$score > 0))
negative <- length(which(singtel.sentiment$score < 0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative,neutral,positive)
output <- as.data.frame(Sentiment,Count)

#plot histogram
qplot(Sentiment,Count,data=output,geom = "histogram", fill=Sentiment,
      binwidth=1,geom_bar(stat="identity"),main="singtel Tweets Sentiment Analysis")

#qplot histogram that works
qplot(singtel.sentiment$score,
      geom="histogram",
      binwidth = 1,
      main = "Singtel SGForum Sentiment Analysis",
      xlim = c(-6, 6),
      xlab = "Score",
      ylab="Count",
      fill=I("red"))