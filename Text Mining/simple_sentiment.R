library("NLP") #text mining (complementer "tm")
library("tm") #text mining
library("syuzhet") #sentiment analysis
library("SnowballC") #text stemming
library("stringi") #preprocessing
library("ROAuth") #authentication through browser
library("rtweet") #access twitter
library("curl") #parse web pages
library("ggplot2") #plot sentiment score
library("wordcloud") #visualize words frequency

#SETUP TWITTER ACCESS -2-
appname <- "TEXT MINING - SENTIMENT ANALYSIS"
key <- "twitter api key"
secret <- "twitter api secret"
access_token <- 'twitter api token'
access_secret <- 'twitter api secret token'

#CREATE TOKEN IN RTWEET
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


#S###################### DATA STREAM & SELECTION ##########################
tweets_a <- search_tweets(q="#blacklivesmatter",
  n=1000,lang = "en")
tweets_b <- search_tweets(q="#xmas",
  n=1000,lang = "en")
tweets_c <- search_tweets(q="facebook",
  n=1000, lang = "en")
tweets_d <- search_tweets(q="statistics",
  n=1000, lang = "en")

########################### PREPROCESSING #################################

a_text<- tweets_a$text
b_text<- tweets_b$text
c_text<- tweets_c$text
d_text<- tweets_d$text

#convert all text to lower case
a_text<- tolower(a_text)
b_text<- tolower(b_text)
c_text<- tolower(c_text)
d_text<- tolower(d_text)

#remove emoticons
a_text<- iconv(a_text,"latin1", 
  "ASCII", sub = " ")
b_text<- iconv(b_text,"latin1", 
  "ASCII", sub = " ")
c_text<- iconv(c_text,"latin1", 
  "ASCII", sub = " ")
d_text<- iconv(d_text,"latin1", 
  "ASCII", sub = " ")

#remove words
a_text <- removeWords(a_text,'blacklivesmatter')
b_text <- removeWords(b_text,'xmas')
c_text <- removeWords(c_text,'facebook')
d_text <- removeWords(d_text,'statistics')

# Replace blank space ("rt")
a_text <- gsub("rt", "", a_text)
b_text <- gsub("rt", "", b_text)
c_text <- gsub("rt", "", c_text)
d_text <- gsub("rt", "", d_text)

# Replace @UserName
a_text <- gsub("@\\w+", "", a_text)
b_text <- gsub("@\\w+", "", b_text)
c_text <- gsub("@\\w+", "", c_text)
d_text <- gsub("@\\w+", "", d_text)


# Remove punctuation
a_text <- gsub("[[:punct:]]", "", a_text)
b_text <- gsub("[[:punct:]]", "", b_text)
c_text <- gsub("[[:punct:]]", "", c_text)
d_text <- gsub("[[:punct:]]", "", d_text)


# Remove links
a_text <- gsub("http\\w+", "", a_text)
b_text <- gsub("http\\w+", "", b_text)
c_text <- gsub("http\\w+", "", c_text)
d_text <- gsub("http\\w+", "", d_text)

# Remove tabs
a_text <- gsub("[ |\t]{2,}", "", a_text)
b_text <- gsub("[ |\t]{2,}", "", b_text)
c_text <- gsub("[ |\t]{2,}", "", c_text)
d_text <- gsub("[ |\t]{2,}", "", d_text)


# Remove blank spaces at the beginning
a_text <- gsub("^ ", "", a_text)
b_text <- gsub("^ ", "", b_text)
c_text <- gsub("^ ", "", c_text)
d_text <- gsub("^ ", "", d_text)

# Remove blank spaces at the end
a_text <- gsub(" $", "", a_text)
b_text <- gsub(" $", "", b_text)
c_text <- gsub(" $", "", c_text)
d_text <- gsub(" $", "", d_text)


#clean up by removing stop words
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
a_text <- stringr::str_replace_all(a_text, 
  stopwords_regex, '')
b_text <- stringr::str_replace_all(b_text, 
  stopwords_regex, '')
c_text <- stringr::str_replace_all(c_text, 
  stopwords_regex, '')
d_text <- stringr::str_replace_all(d_text, 
  stopwords_regex, '')

############################# ANALYZE AND VISUALIZE ########################################

#generate wordcloud
set.seed(4343)
wordcloud(a_text,min.freq = 10,
  colors=brewer.pal(8, "Dark2"),random.color = TRUE,
  max.words = 500)
wordcloud(b_text,min.freq = 10,
  colors=brewer.pal(8, "Dark2"),random.color = TRUE,
  max.words = 500)
wordcloud(c_text,min.freq = 10,
  colors=brewer.pal(8, "Dark2"),random.color = TRUE,
  max.words = 500)
wordcloud(d_text,min.freq = 10,
  colors=brewer.pal(8, "Dark2"),random.color = TRUE,
  max.words = 500)

#sentiment analysis
#getting emotions using in-built function
mysentiment_a<-get_nrc_sentiment((a_text))
mysentiment_b<-get_nrc_sentiment((b_text))
mysentiment_c<-get_nrc_sentiment((c_text))
mysentiment_d<-get_nrc_sentiment((d_text))

#calculating total score for each sentiment
Sentimentscores_a<-data.frame(colSums(mysentiment_a[,]))
Sentimentscores_b<-data.frame(colSums(mysentiment_b[,]))
Sentimentscores_c<-data.frame(colSums(mysentiment_c[,]))
Sentimentscores_d<-data.frame(colSums(mysentiment_d[,]))

names(Sentimentscores_a)<-"Score"
Sentimentscores_a<-cbind("sentiment"=rownames(Sentimentscores_a),
  Sentimentscores_a)
rownames(Sentimentscores_a)<-NULL

names(Sentimentscores_b)<-"Score"
Sentimentscores_b<-cbind("sentiment"=rownames(Sentimentscores_b),
  Sentimentscores_b)
rownames(Sentimentscores_b)<-NULL

names(Sentimentscores_c)<-"Score"
Sentimentscores_c<-cbind("sentiment"=rownames(Sentimentscores_c),
  Sentimentscores_c)
rownames(Sentimentscores_c)<-NULL

names(Sentimentscores_d)<-"Score"
Sentimentscores_d<-cbind("sentiment"=rownames(Sentimentscores_d),
  Sentimentscores_d)
rownames(Sentimentscores_d)<-NULL


#plotting the sentiments with scores
ggplot(data=Sentimentscores_a,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentimen untuk tweets #blacklivesmatter")


ggplot(data=Sentimentscores_b,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentimen untuk tweets #xmas")


ggplot(data=Sentimentscores_c,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentimen untuk tweets tentang facebook")


ggplot(data=Sentimentscores_d,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiment untuk tweets tentang statistics")
