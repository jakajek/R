## TEST ALL 99% DATA
data.jki <- read.csv('gabung-DEMIZ.csv', header = T)
trainIndex <- createDataPartition(data.jk$Senti, p = 0.001, 
                                  list = F, 
                                  times = 1)
tweets_train <- data.jk[trainIndex,]
tweets_test <- data.jk[-trainIndex,]
size.train <- 1:length(tweets_train$Senti)
size.test <- (length(size.train)+1):length(data.jk$Senti)

container.test <- create_container(dtMatrix, data.jki$Senti2, testSize = size.test, virgin=FALSE)
results_lnr.test <- classify_model(container.test, model.lnr)
results_rbf.test <- classify_model(container.test, model.rbf)
write.csv(results_lnr.test, file = 'svm-DEMIZ.ln.csv', row.names=TRUE)
write.csv(results_rbf.test, file = 'svm-DEMIZ.rbf.csv', row.names=TRUE)



###########################################################################################

wewe <- list('saya benci ridwan kamil', 'dibawah pemerintahan jokowi indonesia menjadi negara yg kuat',
             'korupsi masih menjadi musuh utama indonesia', 'dedi yakin akan memenangkan pilgub 2018',
             'pki adalah antek-antek pdip', 'dedi mulyadi dikabarkan memberi mahar pada pdip')

data.jki <- read.csv('gabung-RK.csv', header = T)
prdmtrx <- create_matrix(data.jk['text'], originalMatrix = dtMatrix)
prdsize <- length(data.jk$score)
prdctr <- create_container(prdmtrx, data.jk$Senti, trainSize =1:1  ,testSize =2:prdsize, virgin=T)
rslt.cb <- classify_model(prdctr, model.lnr)


trainIndex <- createDataPartition(data.jk$Senti, p = 0.001, 
                                  list = F, 
                                  times = 1)
tweets_train <- data.jk[trainIndex,]
tweets_test <- data.jk[-trainIndex,]
size.train <- 1:length(tweets_train$Senti)
size.test <- (length(size.train)+1):length(data.jk$Senti)

ensemble <- create_ensembleSummary(analytics_lnr@document_summary)

require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
ap.v<-sort(rowSums(as.matrix(dtMatrix)), decreasing = T)
ap.d<-data.frame(word = names(ap.v), freq = ap.v)
table(ap.d$freq)
pal2<-brewer.pal(8, 'Dark2')
png('wdjk.png', width = 1280, height = 800)
wordcloud(ap.d$word, ap.d$freq, scale = c(8,.2), min.freq = 3, 
          max.words = Inf, random.order = F, rot.per = .15, colors = pal2)
dev.off()


#HAPUS STOPWORDS SUKSES
library(tm)
doc1 <- Corpus(VectorSource(tweets1.1))
doc1 = tm_map(doc1, content_transformer(tolower))
doc1 = tm_map(doc1, removePunctuation)
doc1 = tm_map(doc1, stripWhitespace)
doc1 = tm_map(doc1, removeWords, c(d[[1]]))
doc1 = tm_map(doc1, removeWords, c(d[[2]]))
#doc2 = tm_map(doc1, PlainTextDocument)

dtm <- DocumentTermMatrix(doc1)
trmfreq <- colSums(as.matrix(dtm))
head(trmfreq)

tf <- data.frame(term = names(trmfreq), freq = trmfreq)
tf <- tf[order(-tf[,2]),]
head(tf)

tf[tf$term=='amp']

df1 <- data.frame(text=get("content", doc1))
df2 <- data.frame(text=unlist(sapply(doc1, '[')), stringsAsFactors = F)




####################################################################################
####################################notes###########################################
####################################################################################
f <- content_transformer({function(txt, words, n = 30000L){
  l <- cumsum(nchar(words)+c(0, rep(1, length(words)-1)))
  groups <- cut(1, breaks = seq(1, ceiling(tail(l, 1)/n)*n+1, by = n))
  regexes <- saplly(split(words, groups), function(words) sprintf("(*UCP)\\b(%s)\\b", paste(sort(words, decreasing = TRUE), collapse = "|")))
  for (regex in regexes) txt <- gsub(regex, "", txt, perl = TRUE)
  return(txt)
}})

## extend ceiling terms
group <- 100
n <- length(KStpTerms)
r <- rep(1:ceiling(n/group), each=group)[1:n]
d <- split(KStpTerms, r)
for(i in 1:length(d)){
  clean.tw11 <- removeWords(tweets1.1, c(paste(d[[i]])))
}


## UPDATE R VERSION WITHOUT LOSING PACKAGE ##
#1
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")
#2
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()