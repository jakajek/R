library(wordcloud)
library(RColorBrewer)
library(tm)

library(clValid)
transmatrix<- (t(m))
intern<-clValid(transmatrix,2:10,clMethods=c("hierarchical","kmeans","pam"),validation="internal")
summary(intern)

satu <- (reuters[which(cl$cluster==1)])
dua <- (reuters[which(cl$cluster==2)])


doc2 <- Corpus(VectorSource(dua))
doc2 = tm_map(doc2, tolower)
doc2 = tm_map(doc2, removeNumbers)
doc2 = tm_map(doc2, removeWords, stopwords("en"))
doc2 = tm_map(doc2, removeWords, words("merrychristmasev"))
doc2 = tm_map(doc2, removeWords, words("christma"))
doc2 = tm_map(doc2, removeWords, words("merri"))
doc2 = tm_map(doc2, removeWords, words("fufueu"))
doc2 = tm_map(doc2, removePunctuation)
doc2 = tm_map(doc2, stripWhitespace)


dtm <- DocumentTermMatrix(doc2)
dtm <- removeSparseTerms(dtm, .998)
mat.dtm <- as.matrix(dtm)
sort.dtm <- sort(colSums(mat.dtm), decreasing = T)
df1 <- data.frame(word = names(sort.dtm), freq = sort.dtm)
h <- head(df1, 10)
h
View(h)
set.seed(1983)
wordcloud(words = df1$word, freq = df1$freq, min.freq =1,
          max.words = 2000, random.order = F, rot.per = .35,
          colors = brewer.pal(8, 'Dark2'))

