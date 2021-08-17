#library(tibble)
library(magrittr)
library(magicfor)
library(jsonlite)
library(tm)
library(qdapRegex)
library(anytime)
##Call the data
loc<-c("$kumparancom.json")
rslt<-fromJSON(loc, flatten=TRUE)
str(rslt)
rslt.df<-as.data.frame(rslt)
#View(rslt.df)
n<-nrow(rslt.df)
##Extract, bind and export the data
magic_for(print,silent = T)
for(i in 1:n){
  a1<-rslt.df[[16]][[i]]$created_at
  print(a1)
}
IG_Timestamp<-magic_result_as_vector() %>%
  anydate() %>% 
  as.data.frame()
colnames(IG_Timestamp)<-"Timestamp"

magic_for(print,silent = T)
for(i in 1:n){
  a1<-rslt.df[[16]][[i]]$owner.username
  print(a1)
}
IG_Username<-magic_result_as_vector() %>%
  as.data.frame()
colnames(IG_Username)<-"Username"

magic_for(print,silent = T)
for(i in 1:n){
  a2<-rslt.df[[16]][[i]]$text
  print(a2)
}
IG_Comments<-magic_result_as_vector()%>%
  as.data.frame()
colnames(IG_Comments)<-"Comments"
IG_table1<-cbind(IG_Timestamp,IG_Username,IG_Comments)

#View(IG_table1)
#write.csv(IG_table1,"D:/R/IG_telkomsel_63post.csv")

#Row Excluding if needed
exclude.list <- c("telkomsel")
IG_table1 <- IG_table1[!grepl(exclude.list, IG_table1$Username),]

#write.csv(IG_table1,"D:/R/IG_telkomsel_63post_noreply.csv")

#CLEANING
comment.txt <- IG_table1$Comments

# remove retweet entities
a11 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", comment.txt)
# remove at people
a11 = gsub("@\\w+", " ", a11)
# remove emoticon
a11 = iconv(a11,"latin1", 
            "ASCII", sub = " ")
# lower
a11 = tolower(a11)
# remove punctuation
a11 = gsub("[[:punct:]]", " ", a11)
a11 = gsub("[[:punct:]]", " ", a11)
a11 = gsub("[^0-9A-Za-z///' ]","'" , a11 ,ignore.case = TRUE)
a11 = gsub("''","" , a11 ,ignore.case = TRUE)
# remove numbers
a11 = gsub("[[:digit:]]", " ", a11)
# remove html links
a11 = gsub("http\\w+", " ", a11)

# stop words 1
require(tau)
GetStopWords <- function() {
  stop.words <- readLines("D:/Python/Orange/words/stop.txt", encoding="latin1")
  return(stop.words)
}
KStpTerms <- GetStopWords()
## karena terbatas, maka dipecah dulu
group <- 3400
n <- length(KStpTerms)
r <- rep(1:ceiling(n/group), each=group)[1:n]
d <- split(KStpTerms, r)

a11 <- Corpus(VectorSource(a11))
a11 = tm_map(a11, removeWords, c(d[[1]]))
a11 = tm_map(a11, removeWords, c(d[[2]]))
a11 <- data.frame(text=get("content", a11), check.rows = T)
a11 <- a11$text

# remove words
rmvwrds <- c('lock', 'down', 'lockdown', 'ppkm') #disesuaikan dengan kebutuhan
a11 = removeWords(a11,rmvwrds)
a11 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", comment.txt)
# remove at people
a11 = gsub("@\\w+", " ", a11)
# remove emoticon
a11 = iconv(a11,"latin1", 
            "ASCII", sub = " ")
# remove punctuation
a11 = gsub("[[:punct:]]", " ", a11)
a11 = gsub("[^0-9A-Za-z///' ]","'" , a11 ,ignore.case = TRUE)
#a11 = gsub("''","" , a11 ,ignore.case = TRUE)
a11 = gsub("[[:digit:]]", " ", a11)
# remove html links
a11 = gsub("http\\w+", " ", a11)
# remove nchar <3
a11 = rm_nchar_words(a11,"1,3")
#remove blank spaces at begining
a11 = gsub("^ ", "", a11)
# remove unnecessary spaces
a11 = gsub("[ \t]{2,}", " ", a11)
a11 = gsub("^\\s+|\\s+$", " ", a11)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
a11 = sapply(a11, try.error)

# remove NAs in some_txt
a11 = a11[!is.na(a11)]
names(a11) = "Comments"
a11 <- as.data.frame(a11)

# WORDCLOUD
corpus_tm <- Corpus(VectorSource(a11))
find_freq_terms_fun <- function(corpus_in){
  library(dplyr)
  doc_term_mat <- TermDocumentMatrix(corpus_in)
  freq_terms <- findFreqTerms(doc_term_mat)[1:max(doc_term_mat$nrow)]
  terms_grouped <-
    doc_term_mat[freq_terms,] %>%
    as.matrix() %>%
    rowSums() %>%
    data.frame(Term=freq_terms, Frequency = .) %>%
    arrange(desc(Frequency)) %>%
    mutate(prop_term_to_total_terms=Frequency/nrow(.))
  return(data.frame(terms_grouped))
}
freq_terms <- data.frame(find_freq_terms_fun(corpus_tm))
View(freq_terms)
set.seed(0811)
wordcloud2(freq_terms[,1:2], color = "random-light", backgroundColor = "black", 
           minRotation = -pi/6, maxRotation = -pi/6, minSize = 5, rotateRatio = 1)
