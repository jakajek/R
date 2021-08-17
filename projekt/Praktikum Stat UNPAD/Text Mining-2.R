#install.packages("twitteR")
#install.packages("ROAuth")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
#install.packages("topicmodels")
#install.packages("syuzhet")
library("topicmodels")
library("syuzhet")
library("ROAuth")

#SETUP TWITTER ACCESS
consumer_key <- 'kb84vhMHsxPaqwGIwtcQ9QI11'
consumer_secret <- 'z4EXyu2biPiljzBP6jFdDAbw82B8J8hkJ1r0LiJo12ttxkAapb'
access_token <- '61899761-9DLJKuUSNMJWgxT1EHU572Z3ij7fWh5U4NjZnJJTA'
access_secret <- 'wZ8mJSE9wIarbmq2OQ0GfTz5vOmgEUk2Nm1DroP0FZiR1'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#STREAM DATA
tweets_g <- searchTwitter("#google", n=1000,lang = "en")
tweets_a <- searchTwitter("#amazon", n=1000,lang = "en")
tweets_f <- searchTwitter("#facebook", n=1000,lang = "en")
tweets_tech <- searchTwitter("#technology", n=1000,lang = "en")

#TWEETS UBAH KE DATAFRAME
amazon_tweets <- twListToDF(tweets_a)
google_tweets <- twListToDF(tweets_g)
facebook_tweets <- twListToDF(tweets_f)
tech_tweets <- twListToDF(tweets_tech)

View(amazon_tweets)

###########preprocessing###################

google_text<- google_tweets$text
amazon_text<- amazon_tweets$text
facebook_text<- facebook_tweets$text
tech_text<- tech_tweets$text

#convert all text to lower case
google_text<- tolower(google_text)
amazon_text<- tolower(amazon_text)
facebook_text<- tolower(facebook_text)
tech_text<- tolower(tech_text)

# Replace blank space ("rt")
google_text <- gsub("rt", "", google_text)
amazon_text <- gsub("rt", "", amazon_text)
facebook_text <- gsub("rt", "", facebook_text)
tech_text <- gsub("rt", "", tech_text)

# Replace @UserName
google_text <- gsub("@\\w+", "", google_text)
amazon_text <- gsub("@\\w+", "", amazon_text)
facebook_text <- gsub("@\\w+", "", facebook_text)
tech_text <- gsub("@\\w+", "", tech_text)


# Remove punctuation
google_text <- gsub("[[:punct:]]", "", google_text)
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
facebook_text <- gsub("[[:punct:]]", "", facebook_text)
tech_text <- gsub("[[:punct:]]", "", tech_text)


# Remove links
google_text <- gsub("http\\w+", "", google_text)
amazon_text <- gsub("http\\w+", "", amazon_text)
facebook_text <- gsub("http\\w+", "", facebook_text)
tech_text <- gsub("http\\w+", "", tech_text)

# Remove tabs
google_text <- gsub("[ |\t]{2,}", "", google_text)
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
facebook_text <- gsub("[ |\t]{2,}", "", facebook_text)
tech_text <- gsub("[ |\t]{2,}", "", tech_text)


# Remove blank spaces at the beginning
google_text <- gsub("^ ", "", google_text)
amazon_text <- gsub("^ ", "", amazon_text)
facebook_text <- gsub("^ ", "", facebook_text)
tech_text <- gsub("^ ", "", tech_text)

# Remove blank spaces at the end
google_text <- gsub(" $", "", google_text)
amazon_text <- gsub(" $", "", amazon_text)
facebook_text <- gsub(" $", "", facebook_text)
tech_text <- gsub(" $", "", tech_text)


#clean up by removing stop words
#google_tweets.text.corpus <- tm_map(google_tweets.text.corpus, function(x)removeWords(x,stopwords()))
#amazon_tweets.text.corpus <- tm_map(amazon_tweets.text.corpus, function(x)removeWords(x,stopwords()))
#facebook_tweets.text.corpus <- tm_map(facebook_tweets.text.corpus, function(x)removeWords(x,stopwords()))
#tech_tweets.text.corpus <- tm_map(tech_tweets.text.corpus, function(x)removeWords(x,stopwords()))
google_tweets.text.corpus <- google_text
amazon_tweets.text.corpus <- amazon_text
facebook_tweets.text.corpus <- facebook_text
tech_tweets.text.corpus <- tech_text

library("wordcloud")
#generate wordcloud
set.seed(4343)
wordcloud(google_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(amazon_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(facebook_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
wordcloud(tech_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#sentiment analysis
#getting emotions using in-built function
mysentiment_google<-get_nrc_sentiment((google_text))
mysentiment_amazon<-get_nrc_sentiment((amazon_text))
mysentiment_facebook<-get_nrc_sentiment((facebook_text))
mysentiment_tech<-get_nrc_sentiment((tech_text))

#calculationg total score for each sentiment
Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))
Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))
Sentimentscores_facebook<-data.frame(colSums(mysentiment_facebook[,]))
Sentimentscores_tech<-data.frame(colSums(mysentiment_tech[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

names(Sentimentscores_facebook)<-"Score"
Sentimentscores_facebook<-cbind("sentiment"=rownames(Sentimentscores_facebook),Sentimentscores_facebook)
rownames(Sentimentscores_facebook)<-NULL

names(Sentimentscores_tech)<-"Score"
Sentimentscores_tech<-cbind("sentiment"=rownames(Sentimentscores_tech),Sentimentscores_tech)
rownames(Sentimentscores_tech)<-NULL

#*************************************************************************************

#plotting the sentiments with scores
library(ggplot2)
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")


ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on ecomerce giant AMAZON")


ggplot(data=Sentimentscores_facebook,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Social Netwoking site FACEBOOK")


ggplot(data=Sentimentscores_tech,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech as a whole")


