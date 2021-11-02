library(rtweet)
library(qdapRegex)
library(dplyr)


# Connecting
token <- create_token(app = "ReadVasco",
             consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
             consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
             access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
             access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"))

## Loading tweets for Brazil region near RJ

tweets <- search_tweets('Vasco', since = as.character(Sys.Date()-1),
                        until = as.character(Sys.Date()),n = 10000, retryonratelimit = T,
                        geocode = "-12.6594003,-40.6105266,1300mi")




## Text treatment
tweets <- tweets$text
tweets <- gsub("@\\w+ *", "", tweets)
tweets <- toupper(tweets)
tweets <- tweets %>%tm::removePunctuation() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_squish()
tweets <- iconv(tweets, "latin1", "ASCII", sub="")
tweets <- paste(tweets, collapse = '')
tweets <- unlist(strsplit(tweets, ' '))
stopwords <- read.delim(here::here('misc/stopwords.txt'), header = F)
stopwords$V1 <- toupper(stopwords$V1) %>% stringr::str_squish()
tweets <- data.frame(tweets) %>% filter(!tweets %in% stopwords$V1) %>% group_by(tweets) %>% summarise(n = n()) %>%
  arrange(desc(n))
tweets <- tweets[nchar(tweets$tweets) > 2 & tweets$tweets != 'VASCO',]


## Wordcloud gen

wc <- wordcloud2::wordcloud2(tweets,size =1, color = 'black', backgroundColor = 'white')
htmlwidgets::saveWidget(wc, here::here('misc/1.html'),selfcontained = F)
webshot::webshot(here::here('misc/1.html'), here::here('fig/file.png'),
                 vwidth = 1024, vheight = 512, delay =10)

#post tweet
post_tweet(status = as.character(Sys.Date()), media = here::here('fig/file.png'))

