library(tm)
library(wordcloud)
library(RColorBrewer)

setwd("~/NUS/20162017 Semester 1/BT2101/Project/SGForums Data Mining")
m1<-read.csv("sgforum_m1.csv")
View(m1)
#twitter
setwd("~/NUS/20162017 Semester 1/BT2101/Project")
m1.1<-read.csv("twitter_m1.csv", header=T)
m1.2<-read.csv("twitter_m1w2.csv", header=T)
m1.3<-read.csv("twitter_m1w3.csv", header=T)
m1<- merge(m1.1, m1.2, all=T)
m1<-merge(m1, m1.3, all=T)

#app review
setwd("~/NUS/20162017 Semester 1/BT2101/Project/Mobile App")
m1<-read.csv("m1_app.csv")

#twitter word cloud
m1<-read.csv("twitter_m12016.csv", header=T)

#remove all user tags and urls for Twitter
vs <-gsub("@\\w+ *", "", m1$tweet)
vs <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", vs)

#create Corpus
#sgforum
m1.corpus <- Corpus(VectorSource(m1$forum_post))
#twitter
corpus <- Corpus(VectorSource(vs))
#appreview
m1.corpus<-Corpus(VectorSource(m1$review))

#create term doc matrix
tdm <- TermDocumentMatrix(m1.corpus, control=list(removePunctuation = TRUE, 
                                               stopwords = c("the", "this", "m1", "posted", "originally",'use','still', 'sbs', 'just', 'now', 'can', 'will', 'said', 'get', 'also', 'got', 'nil', 'dont', stopwords('english')), 
                                               removeNumbers=TRUE, tolower=TRUE))

#twitter #removed nonsense words
tdm <- TermDocumentMatrix(corpus, control=list(removePunctuation = TRUE, 
                                               stopwords = c("the", "this", "m1", "m1sg", "singapore","amp",
                                                             stopwords('english')), 
                                               removeNumbers=TRUE, tolower=TRUE))

m<-as.matrix(tdm)

word_freqs = sort(rowSums(m), decreasing = TRUE)
dm= data.frame(word=names(word_freqs), freq=word_freqs)
dm=dm[rownames(dm) != "eduaubdedubu", ]

#generate wordcloud
wordcloud(dm$word, dm$freq, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))

