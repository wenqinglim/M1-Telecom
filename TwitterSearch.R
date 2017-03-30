library(twitteR)
library(ROAuth)

#obtain from twitter app page
consumerKey = "EQFHKvPrEBGpVQpfXsR2b0RAX"
consumerSecret = "NWCqZpyBG32lS74NoGcB589f66AQsAdVQIrWm8iaH3te8jj8SP"
accessToken = "152174048-d2tXWtyOQbA91rwrCd5Wn1FiRTiaJJgZMUIYc84I"
accessSecret = "VaetsOH1zHN8o8p8ejG3Y5EAGZVXT5Bozz96mhJVKoxKD"

#execute before calling any API-related calls
setup_twitter_oauth(consumerKey, consumerSecret, accessToken,accessSecret)

#tweets containing the word Singtel
singtel.list <- searchTwitter("singtel", n=1000)
#tweets by Singtel
singtel.list <-searchTwitter("from:StarHub", n=1000)
#for M1 m1.list <- searchTwitter("m1 Singapore OR @m1Singapore OR to:m1singapore", n=141)
singtel.df <- twListToDF(singtel.list)
write.csv(singtel.df, file="starhub_acct1.csv", row.names=F)
