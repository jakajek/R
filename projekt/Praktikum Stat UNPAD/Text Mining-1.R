#install.packages("tm")
library(tm)
cname <- file.path("D:/JAKA/BAHAN AJAR/DATA MINING/PRAKTIKUM/PRAKTIKUM 5 - TEXT MINING", "texts")   
cname   
dir(cname) 

docs <- VCorpus(DirSource(cname))   
summary(docs)
# LIHAT DETAIL CORPUS
inspect(docs[1])
inspect(docs[2])
# BACA ISI DOKUMEN
writeLines(as.character(docs[1]))

##PREPROCESSING ##
#HAPUS TANDA BACAa#
docs <- tm_map(docs,removePunctuation) 
# baca setelah dipreprocess
writeLines(as.character(docs[1]))

# GUNAKAN PROCEDUR ASCII UTK HAPUS SPECIAL CHAR
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
}
# PERIKSA KEMBALI
writeLines(as.character(docs[1]))
inspect(docs[1])

#HAPUS ANGKA#
docs <- tm_map(docs, removeNumbers)
#PERIKSA KEMBALI
writeLines(as.character(docs[1]))

#HURUF KECIL#
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
#PERIKSA KEMBALI
writeLines(as.character(docs[1]))

#HAPUS STOPWORDS#
length(stopwords("english"))   
stopwords("english") 

docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
#PERIKSA KEMBALI
writeLines(as.character(docs[1]))

#HAPUS KATA TERTENTU#
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   

#GABUNG KATA AGAR MENGANDUNG AKRONIM#
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

#HAPUS AKHIRAN/AWALAN#
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))
#docs <- docs_stc

#HAPUS SPASI GANDA#
docs <- tm_map(docs, stripWhitespace)
#PERIKSA KEMBALI
writeLines(as.character(docs[1]))

#FINALISASI PREPROCESSING#
docs <- tm_map(docs, PlainTextDocument)
#STAGGING DATA#
dtm <- DocumentTermMatrix(docs)   
dtm

##EKSPLORASI DATA##
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)  

#SIMPAN MATRIIX-NYA#
write.csv(m, file="D:/JAKA/BAHAN AJAR/DATA MINING/PRAKTIKUM/PRAKTIKUM 5 - TEXT MINING/DocumentTermMatrix.csv")   

#HAPUS KATA YANG JARANG MUNCUL#
dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms

############################LANJUTAN PRAKTIKUM###################################

#FREKUENSI KATA#
freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20) 
freq <- colSums(as.matrix(dtms))   
freq   
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 
#CARA ALTERNATIF#
findFreqTerms(dtm, lowfreq=50) 
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

#PLOT WORD FREQUENCIES (bar charts)
library(ggplot2)

p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 


#HUBUNGAN ANTAR KATA (ASSOCIATION)
findAssocs(dtm, c("country" , "american"), corlimit=0.85) #korelasi antar kata minimum .83
findAssocs(dtms, "think", corlimit=0.70) # korelasi antar kata masksimum 0.95 

#WORD CLOUDS
#install.packages("RColorBrewer")
library(wordcloud)
#Min 25 Kata
set.seed(142)   
wordcloud(names(freq), freq, min.freq=25) 
#Min 100 Kata
set.seed(142)   
wordcloud(names(freq), freq, min.freq=100)
#BERI WARNA (min 20 kata)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
#BERI WARNA (max 100 kata)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)


#CLUSTERING BERDASARKAN KESAMAAN TERM
dtmss <- removeSparseTerms(dtm, 0.15) # membuat matrix dengan 155 max kol-row kosong
dtmss
#HIERARCHICAL CLUSTERING
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")   # fcoba ganti methodnya dengan ="ward.D"
fit
plot(fit, hang=-1)
#BANTU MELIHAT KELAS
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" definisi banyak klaster akan dibentuk 
rect.hclust(fit, k=6, border="red") #membuat dendogram dengan batas merah antar klaster


#K-MEANS CLUSTERING
#install.packages("fpc")
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

#topic modelling
#install.packages("topicmodels")
library("topicmodels")
tdm<-as.DocumentTermMatrix(dtmss)
lda <- LDA(tdm, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term
#term <- apply(term, MARGIN = 2, paste, collapse = ", ")
# first topic identified for every document (tweet)
#require(data.table) #fore IDate
#topic <- topics(lda, 1)
#topics <- data.frame(topic)
#qplot(date, ..count.., data=topics, geom="density", fill=term[topic], position="stack")
