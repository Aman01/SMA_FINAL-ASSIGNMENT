#GROUP MEMBERS
#AKASH JAISWAL
#SANJONA RAY
#AMAN KOTHARI


#SMA-Assignment/SMA_ASSIGNMENT.R
#https://github.com/Aman01/SMA-Assignment/blob/master/SMA_ASSIGNMENT.R

#Library used for Text and Action Analysis
library(rtweet)
library(twitteR)
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)

#Library used for Network and Location Analysis
library(widyr)
library(tidytext)
library(tidyr)
library(plyr)
library(igraph)

# whatever name you assigned to your created app
appname <- "celebizz"

## api key(application programming interface key)
key <- "v72kUGWHrDFKBT4XmJrFdlaFF"

## api secret key
secret <- "MBawOsmXVxtJEf086ZGz8Gt2hBIgIQx3yUMVmgY14HL7F0cyg7"

OauthToken = "897605731-927aP8civMPyeZCM7uZeVY8XJ944ZNEHtshAHrl3"
OauthaSecret = "vYBPe2GrfC32QTgbXctkMy8qAlmq9myVVbwj7QRRnKsW9"

setup_twitter_oauth(key, secret, OauthToken, OauthaSecret)



chandakochhar_tweets <- search_tweets(q = "chandakochhar", n = 500,
                                      lang = "en",
                                      include_rts = TRUE)
str(chandakochhar_tweets)
head(chandakochhar_tweets$text)

#Remove http elements manually
chandakochhar_tweets$stripped_text <- gsub("http.*","",  chandakochhar_tweets$text)
chandakochhar_tweets$stripped_text <- gsub("https.*","", chandakochhar_tweets$stripped_text)

#Remove punctuation, convert to lowercase, add id for each tweet
chandakochhar_tweets_clean <- chandakochhar_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word,stripped_text)

head(chandakochhar_tweets_clean)

data("stop_words")
head(stop_words)
nrow(chandakochhar_tweets_clean)

#Remove stop words from your list of words
chandakochhar_tweets_words <- chandakochhar_tweets_clean %>%
  anti_join(stop_words)

#There should be fewer words now
nrow(chandakochhar_tweets_words)


head(chandakochhar_tweets_words)


#Find recent tweets with #chandakochhar but ignore retweets
chandakochhar_tweets <- search_tweets("#chandakochhar", n = 500,
                               include_rts = FALSE)
# view top 2 rows of data
head(chandakochhar_tweets, n = 2)
head(chandakochhar_tweets$text)

head(chandakochhar_tweets$screen_name)

# get a list of unique usernames
unique(chandakochhar_tweets$screen_name)

# what users are tweeting with #ICICI
users <- search_users("#ICICI",
                      n = 500)
# just view the first 2 users - the data frame is large!
head(users$text)


# how many locations are represented
length(unique(users$location))

#Removing NA values 
users%>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(5) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

#Location Analysis
myUser= getUser("@vijayshekhar")
myUser
myUser$getScreenName()
myUser$getFriends()
availableTrendLocations()
getTrends(2295411)
tweets= searchTwitter("paytm",n=1000,geocode = "72.87,19.07,2000km",retryOnRateLimit = 200)
tweetsDF2= twListToDF(tweets)
tweets
head(tweetsDF2)
tweetsDF2$longitude= as.numeric(tweetsDF2$longitude)
tweetsDF2$longitude

library(tm)
library(rtweet)
TWEET = search_tweets("#ICICI", n = 200, lang="en" ,include_rts = FALSE)
TWEET$text
# build corpus
corpus <- Corpus(VectorSource(TWEET$text))
inspect(corpus[1:5])
#clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])
# term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
#sentiment analysis
library(syuzhet)
#read file
sentiment <- get_nrc_sentiment(TWEET$text)
# obtain sentiment scores
head(sentiment)

