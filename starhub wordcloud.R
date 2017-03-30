library(tm)
library(wordcloud)
library(RColorBrewer)

setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
starhub<-read.csv("sgforum_starhub.csv")
View(starhub)
#twitter
setwd("~/NUS/20162017 Semester 1/BT2101/Project")
starhub.1<-read.csv("twitter_starhub.csv", header=T)
starhub.2<-read.csv("twitter_starhubw2.csv", header=T)
starhub.3<-read.csv("twitter_starhubw3.csv", header=T)
starhub<- merge(starhub.1, starhub.2, all=T)
starhub<-merge(starhub, starhub.3, all=T)

#app review
setwd("~/NUS/20162017 Semester 1/BT2101/Project/Mobile App")
starhub<-read.csv("starhub_app.csv")

#remove all user tags and urls for Twitter
vs <-gsub("@\\w+ *", "", starhub$text)
vs <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", vs)

#create Corpus
#sgforum
starhub.corpus <- Corpus(VectorSource(starhub$forum_post))
#twitter
corpus <- Corpus(VectorSource(vs))
#appreview
starhub.corpus<-Corpus(VectorSource(starhub$review))

#create term doc matrix
tdm <- TermDocumentMatrix(m1.corpus, control=list(removePunctuation = TRUE, 
                                                  stopwords = c("the", "this", "starhub", "starhubs", "singapore", "amp", "posted", "originally",'use','still', 'sbs', 'just', 'now', 'can', 'will', 'said', 'get', 'also', 'got', 'nil', 'dont', stopwords('english')), 
                                                  removeNumbers=TRUE, tolower=TRUE))

#twitter #removed nonsense words
tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE, 
                                               stopwords = c("the", "this", "starhub", "starhubs", "singapore","amp",
                                                             stopwords('english')), 
                                               removeNumbers=TRUE, tolower=TRUE))
m<-as.matrix(tdm)

word_freqs = sort(rowSums(m), decreasing = TRUE)
dm= data.frame(word=names(word_freqs), freq=word_freqs)
dm=dm[!rownames(dm) %in% c("https.", "eduaubdedubu", "meuuu"), ]

#generate wordcloud
wordcloud(dm$word, dm$freq, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))

