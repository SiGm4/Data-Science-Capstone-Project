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

N_WORKERS <- 1
registerDoParallel(N_WORKERS) 

N_SPLITS <- 4

ptm <- proc.time()
set.seed(1805)
unigrams_final <- data.frame(word=character(),frequency=numeric())
bigrams_final <- data.frame(word=character(),frequency=numeric())
trigrams_final <- data.frame(word=character(),frequency=numeric())
quadgrams_final <- data.frame(word=character(),frequency=numeric())
  
for (i in 1:5){
  print(paste0("Starting iteration ",i))
  tempptm <- proc.time()
  sampleData <- c(sample(twitter,100),sample(blog,100),sample(news,100))
  
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
  
  unigrams_final <- rbind(unigrams_final,unigrams_freq_df)
  
  
  bigrams_final <- rbind(bigrams_final,bigrams_freq_df)
  
  
  trigrams_final <- rbind(trigrams_final,trigrams_freq_df)
  
  
  quadgrams_final <- rbind(quadgrams_final,quadgrams_freq_df)
  
  temptime <- proc.time() - tempptm
  print(temptime[3])
  #gc()
}

agg <- aggregate(unigrams_final$frequency,by=list(unigrams_final$word),sum)
names(agg) = c("word","frequency")
unigrams_final <- agg[order(agg$frequency,decreasing=T),]

agg <- aggregate(bigrams_final$frequency,by=list(bigrams_final$word),sum)
names(agg) = c("word","frequency")
bigrams_final <- agg[order(agg$frequency,decreasing=T),]
print(head(bigrams_final))

agg <- aggregate(trigrams_final$frequency,by=list(trigrams_final$word),sum)
names(agg) = c("word","frequency")
trigrams_final <- agg[order(agg$frequency,decreasing=T),]


agg <- aggregate(quadgrams_final$frequency,by=list(quadgrams_final$word),sum)
names(agg) = c("word","frequency")
quadgrams_final <- agg[order(agg$frequency,decreasing=T),]

proc.time() - ptm


unigrams_final$word <- as.character(unigrams_final$word)
bigrams_final$word <- as.character(bigrams_final$word)
trigrams_final$word <- as.character(trigrams_final$word)
quadgrams_final$word <- as.character(quadgrams_final$word)

saveRDS(unigrams_final,"unigrams.Rdata")
saveRDS(bigrams_final,"bigrams.Rdata")
saveRDS(trigrams_final,"trigrams.Rdata")
saveRDS(quadgrams_final,"quadgrams.Rdata")

unigrams_final <- readRDS("unigrams.Rdata")
bigrams_final <- readRDS("bigrams.Rdata")
trigrams_final <- readRDS("trigrams.Rdata")
quadgrams_final <- readRDS("quadgrams.Rdata")

spl <- strsplit(bigrams_final$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
bi_df <- data.frame(w1,w2,bigrams_final$frequency)

spl <- strsplit(trigrams_final$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
tri_df <- data.frame(w1,w2,w3,trigrams_final$frequency)

spl <- strsplit(quadgrams_final$word," ")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
quad_df <- data.frame(w1,w2,w3,w4,quadgrams_final$frequency)

bi_df$w1 <- as.character(bi_df$w1)
bi_df$w2 <- as.character(bi_df$w2)
tri_df$w1 <- as.character(tri_df$w1)
tri_df$w2 <- as.character(tri_df$w2)
tri_df$w3 <- as.character(tri_df$w3)
quad_df$w1 <- as.character(quad_df$w1)
quad_df$w2 <- as.character(quad_df$w2)
quad_df$w3 <- as.character(quad_df$w3)
quad_df$w4 <- as.character(quad_df$w4)


##QUIZ 2
predict_quad2("a case of")
predict_quad2("would mean the")
predict_quad2("make me the")
predict_quad2("struggling but the")
predict_quad2("date at the")
predict_quad2("be on my")
predict_quad2("in quite some")
predict_quad2("with his little")
predict_quad2("faith during the")
predict_quad2("you must be")
predict_tri2("case of")
predict_tri2("mean the")
predict_tri2("me the")
predict_tri2("but the")
predict_tri2("at the")
predict_tri2("on my")
predict_tri2("quite some")
predict_tri2("his little")
predict_tri2("during the")
predict_tri2("must be")

##QUIZ 3
predict_quad("you live and",c("give","eat","die","sleep"))
predict_quad("me about his",c("marital","horticultural","financial","spiritual"))
predict_quad("arctic monkeys this",c("month","morning","weekend","decade"))
predict_quad("helps reduce your",c("hunger","stress","sleepiness","happiness"))
predict_quad("to take a",c("walk","picture","look","minute"))
predict_quad("to settle the",c("case","matter","incident","account"))
predict_quad("groceries in each",c("toe","arm","finger","hand"))
predict_quad("bottom to the",c("top","side","center","middle"))
predict_quad("bruises from playing",c("inside","outside","daily","weekly"))
predict_quad("of Adam Sandler",c("novels","stories","movies","pictures"))
predict_tri("live and",c("give","eat","die","sleep"))
predict_tri("about his",c("marital","horticultural","financial","spiritual"))
predict_tri("monkeys this",c("month","morning","weekend","decade"))
predict_tri("reduce your",c("hunger","stress","sleepiness","happiness"))
predict_tri("take a",c("walk","picture","look","minute"))
predict_tri("settle the",c("case","matter","incident","account"))
predict_tri("in each",c("toe","arm","finger","hand"))
predict_tri("to the",c("top","side","center","middle"))
predict_tri("from playing",c("inside","outside","daily","weekly"))
predict_tri("Adam Sandler",c("novels","stories","movies","pictures"))
predict_bi("and",c("give","eat","die","sleep"))
predict_bi("his",c("marital","horticultural","financial","spiritual"))
predict_bi("this",c("month","morning","weekend","decade"))
predict_bi("your",c("hunger","stress","sleepiness","happiness"))
predict_bi("a",c("walk","picture","look","minute"))
predict_bi("the",c("case","matter","incident","account"))
predict_bi("each",c("toe","arm","finger","hand"))
predict_bi("the",c("top","side","center","middle"))
predict_bi("playing",c("inside","outside","daily","weekly"))
predict_bi("Sandler",c("novels","stories","movies","pictures"))
