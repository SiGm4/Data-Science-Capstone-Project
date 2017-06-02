if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
}
download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt","bad-words.txt")
#Loading files 
#library(readr)
library(tm)
library(RWeka)
library(ggplot2)
#library(qdap)
library(SnowballC)
library(stringi)
#if (!file.exists("Coursera-SwiftKey.zip")) {
#  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","Coursera-SwiftKey.zip")
#  unzip("Coursera-SwiftKey.zip")
#}
#twitter <- read_lines("./capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
#blog <- read_lines("./capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
#news <- read_lines("./capstone/Coursera-SwiftKey/final/en_US/en_US.news.txt")
twitter.url <- "./Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
blog.url <- "./Coursera-SwiftKey/final/en_US/en_US.blogs.txt"
news.url <- "./Coursera-SwiftKey/final/en_US/en_US.news.txt"
twitter <- readLines(twitter.url, skipNul = TRUE, encoding = "UTF-8")
blog <- readLines(blog.url, skipNul = TRUE, encoding = "UTF-8")
news.file <- file(news.url,"rb")
news <- readLines(news.file, skipNul = TRUE, encoding = "UTF-8")
close(news.file)

create_summary_table <- function(twitter,blog,news){
    stats <- data.frame(source = c("twitter","blog","news"),
                        arraySizeMB = c(object.size(twitter)/1024^2,object.size(blog)/1024^2,object.size(news)/1024^2),
                        fileSizeMB = c(file.info(twitter.url)$size/1024^2,file.info(blog.url)$size/1024^2,file.info(news.url)$size/1024^2),
                        lineCount = c(length(twitter),length(blog),length(news)),
                        wordCount = c(sum(stri_count_words(twitter)),sum(stri_count_words(blog)),sum(stri_count_words(news))),
                        charCount = c(stri_stats_general(twitter)[3],stri_stats_general(blog)[3],stri_stats_general(news)[3])
    )
    print(stats)
}
create_summary_table(twitter,blog,news)

set.seed(1805)
sampleData <- c(sample(twitter,3000),sample(blog,3000),sample(news,3000))
corpus <- VCorpus(VectorSource(sampleData))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
#Cleaning all non ASCII characters
corpus <- tm_map(corpus,toSpace,"[^[:graph:]]")
#Transforming all data to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
#Deleting all English stopwords and any stray letters left my the non-ASCII removal
#corpus <- tm_map(corpus,removeWords,c(stopwords("english"),letters))
#Removing Punctuation
corpus <- tm_map(corpus,removePunctuation)
#Removing Numbers
corpus <- tm_map(corpus,removeNumbers)
#Removing Profanities
profanities = readLines('bad-words.txt')
corpus <- tm_map(corpus, removeWords, profanities)
#Removing all stray letters left by the last two calls
corpus <- tm_map(corpus,removeWords,letters)
#Striping all extra whitespace
corpus <- tm_map(corpus,stripWhitespace)



unigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
unigrams <- DocumentTermMatrix(corpus, control = list(tokenize = unigramTokenizer))
unigrams <- removeSparseTerms(unigrams,0.9998)

BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
bigrams <- removeSparseTerms(bigrams,0.9998)

TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
trigrams <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
trigrams <- removeSparseTerms(trigrams,0.9998)

QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}
quadgrams <- DocumentTermMatrix(corpus, control = list(tokenize = QuadgramTokenizer))
quadgrams <- removeSparseTerms(quadgrams,0.9998)


freqTerms <- findFreqTerms(unigrams,lowfreq = 1)
unigrams_frequency <- sort(colSums(as.matrix(unigrams[,freqTerms])),decreasing = TRUE)
unigrams_freq_df <- data.frame(word = names(unigrams_frequency), frequency = unigrams_frequency)
#wordcloud(unigrams_freq_df$word,unigrams_freq_df$frequency,scale=c(4,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

freqTerms <- findFreqTerms(bigrams,lowfreq = 1)
bigrams_frequency <- sort(colSums(as.matrix(bigrams[,freqTerms])),decreasing = TRUE)
bigrams_freq_df <- data.frame(word = names(bigrams_frequency), frequency = bigrams_frequency)
#wordcloud(bigrams_freq_df$word,bigrams_freq_df$frequency,scale=c(3,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

freqTerms <- findFreqTerms(trigrams,lowfreq = 1)
trigrams_frequency <- sort(colSums(as.matrix(trigrams[,freqTerms])),decreasing = TRUE)
trigrams_freq_df <- data.frame(word = names(trigrams_frequency), frequency = trigrams_frequency)
#wordcloud(trigrams_freq_df$word,trigrams_freq_df$frequency,scale=c(3,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

freqTerms <- findFreqTerms(quadgrams,lowfreq = 1)
quadgrams_frequency <- sort(colSums(as.matrix(quadgrams[,freqTerms])),decreasing = TRUE)
quadgrams_freq_df <- data.frame(word = names(quadgrams_frequency), frequency = quadgrams_frequency)

####Most common unigrams
gu <- ggplot(head(unigrams_freq_df,15),aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Unigram") + ylab("Frequency") +labs(title="Most common unigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gu

####Most common bigrams
gb <- ggplot(head(bigrams_freq_df,15),aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Bigram") + ylab("Frequency") +labs(title="Most common bigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gb

####Most common trigrams
gc <- ggplot(head(trigrams_freq_df,15),aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Trigram") + ylab("Frequency") +labs(title="Most common trigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gc

####Most common quadrams
gq <- ggplot(head(quadgrams_freq_df,15),aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Quadgram") + ylab("Frequency") +labs(title="Most common quadgrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gq

unigrams_freq_df$word <- as.character(unigrams_freq_df$word)
bigrams_freq_df$word <- as.character(bigrams_freq_df$word)
trigrams_freq_df$word <- as.character(trigrams_freq_df$word)
quadgrams_freq_df$word <- as.character(quadgrams_freq_df$word)

saveRDS(unigrams_freq_df,"unigrams.Rdata")
saveRDS(bigrams_freq_df,"bigrams.Rdata")
saveRDS(trigrams_freq_df,"trigrams.Rdata")
saveRDS(quadgrams_freq_df,"quadgrams.Rdata")

unigrams_freq_df <- readRDS("unigrams.Rdata")
bigrams_freq_df <- readRDS("bigrams.Rdata")
trigrams_freq_df <- readRDS("trigrams.Rdata")
quadgrams_freq_df <- readRDS("quadgrams.Rdata")

rm(news,blog,twitter)

spl <- strsplit(bigrams_freq_df$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
bi_df <- data.frame(w1,w2,bigrams_freq_df$frequency)

spl <- strsplit(trigrams_freq_df$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
tri_df <- data.frame(w1,w2,w3,trigrams_freq_df$frequency)

spl <- strsplit(quadgrams_freq_df$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
quad_df <- data.frame(w1,w2,w3,w4,quadgrams_freq_df$frequency)

predict_bi <- function(str){
  input <- unlist(strsplit(str," "))
  as.character(bi_df[(bi_df$w1==input[1]),][,2][1])
}
predict_tri <- function(str){
  input <- unlist(strsplit(str," "))
  as.character(tri_df[(tri_df$w1==input[1] & tri_df$w2==input[2]),][,3][1])
}
predict_quad <- function(str){
  input <- unlist(strsplit(str," "))
  as.character(quad_df[(quad_df$w1==input[1] & quad_df$w2==input[2] & quad_df$w3==input[3]),][,4][1])
}

predict_bi("trying")
predict_tri("thank you")
predict_quad("you just have")

predict <- function(str){
  #print(str)
  wordcount <- length(unlist(strsplit(str," ")))
  result <- ""
  if (wordcount == 1){
    if (is.na(predict_bi(str))){
      result = "the"
    }
    else {
      result <- predict_bi(str)
    }
  }
  else if (wordcount == 2){
    if (is.na(predict_tri(str))){
      if (is.na(predict_bi(str))){
        result = "the"
      }
      else {
        result <- predict_bi(str)
      }
    }
    else{
      result <- predict_tri(str)
    }
  }
  else if (wordcount >= 3){
    new_str <- paste(tail(unlist(strsplit(str," ")),3),collapse=" ")
    if (is.na(predict_quad(new_str))){
      if (is.na(predict_tri(new_str))){
        if (is.na(predict_bi(new_str))){
          result = "the"
        }
        else {
          result <- predict_bi(new_str)
        }
      }
      else{
        result <- predict_tri(new_str)
      }
    }
    else{
      result <- predict_quad(new_str)
    }
  }
  result
}


test_predicts <- function(str){
  wordcount <- length(unlist(strsplit(str," ")))
  result <-  NA
  if (wordcount==1){
    print(str)
    result <- predict_bi(str)
  }
  else if (wordcount == 2){
    new_str <- tail(unlist(strsplit(str," ")),1)
    print(new_str)
    result <- c(predict_bi(str),predict_tri(new_str))
  }
  else if (wordcount >= 3){
    new_str1 <- tail(unlist(strsplit(str," ")),1)
    new_str2 <- paste(tail(unlist(strsplit(str," ")),2),collapse=" ")
    new_str3 <- paste(tail(unlist(strsplit(str," ")),3),collapse=" ")
    print(new_str1)
    print(new_str2)
    print(new_str3)
    result <- c(predict_bi(new_str1),predict_tri(new_str2),predict_quad(new_str3))
  }
  result
}



