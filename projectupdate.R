##CODE TO LOAD MANY LINES
twitter.url <- "./Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
blog.url <- "./Coursera-SwiftKey/final/en_US/en_US.blogs.txt"
news.url <- "./Coursera-SwiftKey/final/en_US/en_US.news.txt"
twitter.file <- file(twitter.url,"rb")
twitter <- readLines(twitter.file, skipNul = TRUE, encoding = "UTF-8")
close(twitter.file)
blog.file <- file(blog.url,"rb")
blog <- readLines(blog.file, skipNul = TRUE, encoding = "UTF-8")
close(blog.file)
news.file <- file(news.url,"rb")
news <- readLines(news.file, skipNul = TRUE, encoding = "UTF-8")
close(news.file)

set.seed(1805)
sampleData <- c(sample(twitter,1250),sample(blog,1250),sample(news,1250))

corpus <- toCorpus(sampleData)

unigrams <- DocumentTermMatrix(corpus, control = list(tokenize = unigramTokenizer))
#unigrams <- removeSparseTerms(unigrams,0.999)
bigrams <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
#bigrams <- removeSparseTerms(bigrams,0.999)
trigrams <- DocumentTermMatrix(corpus, control = list(tokenize = TrigramTokenizer))
#trigrams <- removeSparseTerms(trigrams,0.999)
quadgrams <- DocumentTermMatrix(corpus, control = list(tokenize = QuadgramTokenizer))
#quadgrams <- removeSparseTerms(quadgrams,0.999)
freqTerms <- findFreqTerms(unigrams,lowfreq = 1)
unigrams_frequency <- sort(colSums(as.matrix(unigrams[,freqTerms])),decreasing = TRUE)
unigrams_freq_df <- data.frame(word = names(unigrams_frequency), frequency = unigrams_frequency)

freqTerms <- findFreqTerms(bigrams,lowfreq = 1)
bigrams_frequency <- sort(colSums(as.matrix(bigrams[,freqTerms])),decreasing = TRUE)
bigrams_freq_df <- data.frame(word = names(bigrams_frequency), frequency = bigrams_frequency)

freqTerms <- findFreqTerms(trigrams,lowfreq = 1)
trigrams_frequency <- sort(colSums(as.matrix(trigrams[,freqTerms])),decreasing = TRUE)
trigrams_freq_df <- data.frame(word = names(trigrams_frequency), frequency = trigrams_frequency)

freqTerms <- findFreqTerms(quadgrams,lowfreq = 1)
quadgrams_frequency <- sort(colSums(as.matrix(quadgrams[,freqTerms])),decreasing = TRUE)
quadgrams_freq_df <- data.frame(word = names(quadgrams_frequency), frequency = quadgrams_frequency)

#merged <- rbind(x,y)
#aggregate(merged$no,by=list(merged$type),sum)