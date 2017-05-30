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
corpus <- tm_map(corpus,removeWords,c(stopwords("english"),letters))
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
#unigrams <- removeSparseTerms(unigrams,0.99)

BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
#bigrams <- removeSparseTerms(bigrams,0.999)

TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
trigrams <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
#trigrams <- removeSparseTerms(trigrams,0.9999)


freqTerms <- findFreqTerms(unigrams,lowfreq = 1)
unigrams_frequency <- sort(colSums(as.matrix(unigrams[,freqTerms])),decreasing = TRUE)
unigrams_freq_df <- data.frame(word = names(unigrams_frequency), frequency = unigrams_frequency)
#wordcloud(unigrams_freq_df$word,unigrams_freq_df$frequency,scale=c(4,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

freqTerms <- findFreqTerms(bigrams,lowfreq = 2)
bigrams_frequency <- sort(colSums(as.matrix(bigrams[,freqTerms])),decreasing = TRUE)
bigrams_freq_df <- data.frame(word = names(bigrams_frequency), frequency = bigrams_frequency)
#wordcloud(bigrams_freq_df$word,bigrams_freq_df$frequency,scale=c(3,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

freqTerms <- findFreqTerms(trigrams,lowfreq = 2)
trigrams_frequency <- sort(colSums(as.matrix(trigrams[,freqTerms])),decreasing = TRUE)
trigrams_freq_df <- data.frame(word = names(trigrams_frequency), frequency = trigrams_frequency)
#wordcloud(trigrams_freq_df$word,trigrams_freq_df$frequency,scale=c(3,.1), colors = brewer.pal(7, "Dark2"), random.order = TRUE, random.color = TRUE, rot.per = 0.35)

####Most common unigrams
gu <- ggplot(unigrams_freq_df,aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Unigram") + ylab("Frequency") +labs(title="Most common unigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gu

####Most common bigrams
gb <- ggplot(bigrams_freq_df,aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Bigram") + ylab("Frequency") +labs(title="Most common bigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gb

####Most common trigrams
gc <- ggplot(trigrams_freq_df,aes(x=reorder(word,-frequency),y=frequency))+geom_bar(stat="identity",fill="darkolivegreen4") + xlab("Trigram") + ylab("Frequency") +labs(title="Most common trigrams") + theme(axis.text.x=element_text(angle=55, hjust=1))
gc