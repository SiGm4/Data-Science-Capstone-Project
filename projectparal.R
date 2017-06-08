################################
ptm <- proc.time()

library(tm)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(stringi)
library(doParallel)
library(text2vec)

proc.time() - ptm
################################
ptm <- proc.time()

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

proc.time()-ptm
###################################
ptm <- proc.time()

all <- c(twitter,blog,news)
rm(twitter,twitter.url,twitter.file,blog,blog.url,blog.file,news,news.url,news.file)

proc.time() - ptm
###################################
ptm <- proc.time()

removeConfounders <- function(x) {
  x <- gsub("-", " ", x)
  x <- gsub(":", " ", x)
  x <- gsub(" -", " ", x)
  x <- gsub("- ", " ", x)
  x <- gsub(";", " ", x)
  x <- gsub("won't", "will not", x)
  x <- gsub("can't", "cannot", x)
  x <- gsub("'re", " are", x)
  x <- gsub("'ve", " have", x)
  x <- gsub("what's", "what is", x)
  x <- gsub("n't", " not", x)
  x <- gsub("'d", " would", x)
  x <- gsub("'ll", " will", x)
  x <- gsub("'m", " am", x)
}

# define tokenizer and stemmer function 

set.seed(1805)
all2 <- sample(all,500000)
rm(all)
toSpace <- function(x){x <- gsub("[^[:graph:]]", " ", x)}
# apply lower case transformation and stemmer to all the text data
train_tokens <- all2 %>%
  toSpace %>%
  removeConfounders %>%
  removePunctuation %>%
  tolower %>% 
  removeNumbers %>%
  stripWhitespace

rm(all2)
proc.time() - ptm
####################################
ptm <- proc.time()

saveRDS(train_tokens,"train_tokens.Rdata")
train_tokens <- readRDS("train_tokens.Rdata")

proc.time()-ptm
####################################
ptm <- proc.time()

# register parallel backend 
N_WORKERS <- 4
registerDoParallel(N_WORKERS) 

# define number of splits 
N_SPLITS <- 4

jobs <- train_tokens %>%
  split_into(N_SPLITS) %>%
  lapply(itoken)
rm(train_tokens)
# unigram text2vec object
vocab <- create_vocabulary(jobs, ngram = c(1L, 1L))
pruned_vocab <- prune_vocabulary(vocab,term_count_min = 10)
rm(vocab)
# bigram text2vec object
vocab2 <- create_vocabulary(jobs, ngram = c(2L, 2L))
pruned_vocab2 <- prune_vocabulary(vocab2,term_count_min = 4)
rm(vocab2)
# trigram text2vec object
vocab3 <- create_vocabulary(jobs, ngram = c(3L, 3L))
pruned_vocab3 <- prune_vocabulary(vocab3,term_count_min = 2)
rm(vocab3)
# quadgram text2vec object
vocab4 <- create_vocabulary(jobs, ngram = c(4L, 4L))
pruned_vocab4 <- prune_vocabulary(vocab4,term_count_min = 2)
rm(vocab4)
# pentagram text2vec object
vocab5 <- create_vocabulary(jobs, ngram = c(5L, 5L))
#pruned_vocab5 <- prune_vocabulary(vocab5,term_count_min = 2)
#rm(vocab5)
# ectagram text2vec object
vocab6 <- create_vocabulary(jobs, ngram = c(6L, 6L))
#pruned_vocab6 <- prune_vocabulary(vocab6,term_count_min = 2)
#rm(vocab6)
proc.time() - ptm
gc()
####################################
ptm <- proc.time()
freqr <- pruned_vocab[1]
freqr <- data.frame(freqr)
freqr <- freqr[, 1:2]
freqr <- freqr %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

freqr2 <- pruned_vocab2[1]
freqr2 <- data.frame(freqr2)
freqr2 <- freqr2[, 1:2]
freqr2 <- freqr2 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

freqr3 <- pruned_vocab3[1]
freqr3 <- data.frame(freqr3)
freqr3 <- freqr3[, 1:2]
freqr3 <- freqr3 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

freqr4 <- pruned_vocab4[1]
freqr4 <- data.frame(freqr4)
freqr4 <- freqr4[, 1:2]
freqr4 <- freqr4 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

freqr5 <- vocab5[1]
freqr5 <- data.frame(freqr5)
freqr5 <- freqr5[, 1:2]
freqr5 <- freqr5 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

freqr6 <- vocab6[1]
freqr6 <- data.frame(freqr6)
freqr6 <- freqr6[, 1:2]
freqr6 <- freqr6 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)

rm(pruned_vocab)
rm(pruned_vocab2)
rm(pruned_vocab3)
rm(pruned_vocab4)
rm(vocab5)
rm(vocab6)

proc.time()-ptm
####################################
ptm <- proc.time()

saveRDS(freqr,"unigrams.Rdata")
saveRDS(freqr2,"bigrams.Rdata")
saveRDS(freqr3,"trigrams.Rdata")
saveRDS(freqr4,"quadgrams.Rdata")
saveRDS(freqr5,"pentagrams.Rdata")
saveRDS(freqr6,"ectagrams.Rdata")

freqr <- readRDS("unigrams.Rdata")
freqr2 <- readRDS("bigrams.Rdata")
freqr3 <- readRDS("trigrams.Rdata")
freqr4 <- readRDS("quadgrams.Rdata")
freqr5 <- readRDS("pentagrams.Rdata")
freqr6 <- readRDS("ectagrams.Rdata")

proc.time()-ptm
####################################
ptm <- proc.time()

spl <- strsplit(freqr2$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
bi_df <- data.frame(w1,w2,freqr2$frequency)
rm(w1,w2,freqr2)

spl <- strsplit(freqr3$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
tri_df <- data.frame(w1,w2,w3,freqr3$frequency)
rm(w1,w2,w3,freqr3)

spl <- strsplit(freqr4$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
quad_df <- data.frame(w1,w2,w3,w4,freqr4$frequency)
rm(w1,w2,w3,w4,freqr4)

spl <- strsplit(freqr5$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
w5 <- sapply(spl,"[[",5)
penta_df <- data.frame(w1,w2,w3,w4,w5,freqr5$frequency)
rm(spl,w1,w2,w3,w4,w5,freqr5)
gc()

spl <- strsplit(freqr6$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
w5 <- sapply(spl,"[[",5)
w6 <- sapply(spl,"[[",6)
ecta_df <- data.frame(w1,w2,w3,w4,w5,w6,freqr6$frequency)
rm(spl,w1,w2,w3,w4,w5,w6,freqr6)

proc.time()-ptm
####################################
ptm <- proc.time()

saveRDS(bi_df,"bigrams.Rdata")
saveRDS(tri_df,"trigrams.Rdata")
saveRDS(quad_df,"quadgrams.Rdata")
saveRDS(penta_df,"pentagrams.Rdata")
saveRDS(ecta_df,"ectagrams.Rdata")

bi_df <- readRDS("bigrams.Rdata")
tri_df <- readRDS("trigrams.Rdata")
quad_df <- readRDS("quadgrams.Rdata")
penta_df <- readRDS("pentagrams.Rdata")
ecta_df <- readRDS("ectagrams.Rdata")

proc.time()-ptm
####################################
