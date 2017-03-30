library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

setwd("~/NUS/20162017 Semester 1/BT2101/Project")
starhub.1<-read.csv("twitter_starhub.csv", header=T)
starhub.2<-read.csv("twitter_starhubw2.csv", header=T)
starhub.3<-read.csv("twitter_starhubw3.csv", header=T)
starhub<- merge(starhub.1, starhub.2, all=T)
starhub<-merge(starhub, starhub.3, all=T)

write.csv(starhub, "twitter_starhubALL.csv")
starhub.txt<-read.csv("twitter_starhubALL.csv")
#sgforums
setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
starhub.txt<-read.csv("sgforum_starhub.csv", header=T)
starhub.txt$X_input=NULL
starhub.txt<-na.omit(starhub.txt)

#twitter
starhub.txt = starhub.txt$text
#sgforums
starhub.txt=starhub.txt$forum_post

#remove @tags, urls, and emojis
starhub.txt <-gsub("@\\w+ *", "", starhub.txt)
starhub.txt <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", starhub.txt)
starhub.txt <- iconv(starhub.txt, 'UTF-8', 'ASCII')

#SGForum Remove originally posted by tag
starhub.txt <- gsub("Originally posted by", "", starhub.txt)
starhub.txt<-gsub("starhub", "", starhub.txt)

#remove stopwords
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
starhub.txt <- rm_words(starhub.txt, tm::stopwords("en"))

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
    sentence = gsub('starhub', '', sentence)
    
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

starhub.sentiment<-score.sentiment(starhub.txt, poswords, negwords)
starhub.sentiment[starhub.sentiment=="NA"]<- NA
starhub.sentiment<-na.omit(starhub.sentiment)

neutral <- length(which(starhub.sentiment$score == 0))
positive <- length(which(starhub.sentiment$score > 0))
negative <- length(which(starhub.sentiment$score < 0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative,neutral,positive)
output <- as.data.frame(Sentiment,Count)

#plot histogram
qplot(Sentiment,Count,data=output,geom = "histogram", fill=Sentiment,
      binwidth=1,geom_bar(stat="identity"),main="starhub Tweets Sentiment Analysis")

#qplot histogram that works
qplot(starhub.sentiment$score,
      geom="histogram",
      binwidth = 1,
      main = "Starhub SG Forum Sentiment Analysis",
      xlim = c(-6, 6),
      xlab = "Score",
      ylab="Count",
      fill=I("green"))