library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)

setwd("~/NUS/20162017 Semester 1/BT2101/Project")
m1.1<-read.csv("twitter_m1.csv", header=T)
m1.2<-read.csv("twitter_m1w2.csv", header=T)
m1.3<-read.csv("twitter_m1w3.csv", header=T)
m1<- merge(m1.1, m1.2, all=T)
m1<-merge(m1, m1.3, all=T)

write.csv(m1, "twitter_m1ALL.csv")
m1.txt<-read.csv("twitter_m1ALL.csv")
#sgforums
setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
m1.txt<-read.csv("sgforum_m1.csv", header=T)
m1.txt$X_input=NULL
m1.txt<-na.omit(m1.txt)

#twitter
m1.txt = m1.txt$text
#sgforums
m1.txt=m1.txt$forum_post

#remove @tags, urls, and emojis
m1.txt <-gsub("@\\w+ *", "", m1.txt)
m1.txt <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", m1.txt)
m1.txt <- iconv(m1.txt, 'UTF-8', 'ASCII')

#SGForum Remove originally posted by tag
m1.txt <- gsub("Originally posted by", "", m1.txt)
m1.txt<-gsub("M1", "", m1.txt)

#remove stopwords
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
m1.txt <- rm_words(m1.txt, tm::stopwords("en"))

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
    sentence = gsub('m1', '', sentence)
    
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

#Lexicon taken from https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
poswords <- scan("positive-words.txt", what="character", comment.char=";")
negwords <- scan("negative-words.txt", what="character", comment.char=";")

m1.sentiment<-score.sentiment(m1.txt, poswords, negwords)
m1.sentiment[m1.sentiment=="NA"]<- NA
m1.sentiment<-na.omit(m1.sentiment)

neutral <- length(which(m1.sentiment$score == 0))
positive <- length(which(m1.sentiment$score > 0))
negative <- length(which(m1.sentiment$score < 0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative,neutral,positive)
output <- as.data.frame(Sentiment,Count)

#qplot histogram that works
qplot(m1.sentiment$score,
      geom="histogram",
      binwidth = 1,
      main = "M1 SG Forum Sentiment Analysis",
      xlim = c(-6, 6),
      xlab = "Score",
      ylab = "Count",
      fill=I("orange"))