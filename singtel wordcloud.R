library(tm)
library(wordcloud)
library(RColorBrewer)

#sgforum
setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
singtel<-read.csv("sgforum_singtel.csv")
View(singtel)
#twitter
setwd("~/NUS/20162017 Semester 1/BT2101/Project")
singtel1<-read.csv("twitter_singtel.csv", header=T)
singtel2<-read.csv("twitter_singtelw2.csv", header=T)
singtel3<-read.csv("twitter_singtelw3.csv", header=T)
singtel<- merge(singtel1, singtel2, all=T)
singtel<-merge(singtel, singtel3, all=T)

#remove all user tags for Twitter
vs <-gsub("@\\w+ *", "", singtel$text)
vs <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", vs)

#create Corpus
#sgforum
corpus <- Corpus(VectorSource(singtel$forum_post))
#twitter
corpus <- Corpus(VectorSource(vs))

#create term doc matrix
#sgforum
tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE, 
                                                  stopwords = c("the", "this", "singtel", "singapore", 'singtels', "Singtel", "posted", "originally",'use','still', 'sbs', 'just', 'now', 'can', 'will', 'said', 'get', 'also', 'got', 'nil', 'dont', stopwords('english')), 
                                                  removeNumbers=TRUE, tolower=TRUE))
#twitter #removed set of tagalog stopwords
tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE, 
                                               stopwords = c("the", "this", "singtel", "singtels", 
                                                             "ang", "aldubselflessacts", "ulit", "natin", 
                                                             "iyam", "ulitin", "teamabroad", "bibis.", "amp",
                                                             stopwords('english')), 
                                               removeNumbers=TRUE, tolower=TRUE))

m<-as.matrix(tdm)

word_freqs = sort(rowSums(m), decreasing = TRUE)
dm= data.frame(word=names(word_freqs), freq=word_freqs)

#generate wordcloud
wordcloud(dm$word, dm$freq, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
