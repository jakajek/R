library(rtweet)
twitter_token <- create_token(app = "TEXT MINING - SENTIMENT ANALYSIS", # whatever you named app
                              consumer_key = "kb84vhMHsxPaqwGIwtcQ9QI11",
                              consumer_secret = "z4EXyu2biPiljzBP6jFdDAbw82B8J8hkJ1r0LiJo12ttxkAapb")
## LOKASI ##
bdg<-lookup_coords('bandung, west java', 'country:indonesia')
jabar<-lookup_coords("west java")

## SEARCH ##
tw1<-search_tweets('@ridwankamil', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = 'recent', retryonratelimit = T)
tw2<-search_tweets('@Deddy_Mizwar_', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
tw3<-search_tweets('@DediMulyadi71', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
tw4<-search_tweets('@mayjensudrajat', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
tw5<-search_tweets('@antoncharlian', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = 'recent', retryonratelimit = T)
tw6<-search_tweets('@tbhasanuddin', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
tw7<-search_tweets('@uuruzhan', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
tw8<-search_tweets('@syaikhu_ahmad', token = twitter_token, include_rts = F,
                   n=100000, geocode = jabar, type = "recent", retryonratelimit = T)
## SAVE ##
tw1.user<-users_data(tw1)
setwd("D:/R Mighty/")
save_as_csv(tw1, file_name = 'PILKADA/RK5.csv')
save_as_csv(tw2, file_name = 'PILKADA/DEMIZ5.csv')
save_as_csv(tw3, file_name = 'PILKADA/DEMUL5.csv')
save_as_csv(tw4, file_name = 'PILKADA/SUDRAJAT5.csv')
save_as_csv(tw5, file_name = 'PILKADA/ANTON5.csv')
save_as_csv(tw6, file_name = 'PILKADA/TB HASAN5.csv')
save_as_csv(tw7, file_name = 'PILKADA/UU5.csv')
save_as_csv(tw8, file_name = 'PILKADA/SYAIKHU5.csv')
