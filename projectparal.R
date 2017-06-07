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
  x <- gsub("'er", " are", x)
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
  removePunctuation %>%
  removeConfounders %>%
  tolower %>% 
  removeNumbers %>%
  stripWhitespace

rm(all2)
proc.time() - ptm
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

# bigram text2vec object
vocab2 <- create_vocabulary(jobs, ngram = c(2L, 2L))

# trigram text2vec object
vocab3 <- create_vocabulary(jobs, ngram = c(3L, 3L))

# quadgram text2vec object
vocab4 <- create_vocabulary(jobs, ngram = c(4L, 4L))

proc.time() - ptm
####################################
ptm <- proc.time()
pruned_vocab <- prune_vocabulary(vocab,term_count_min = 10)

pruned_vocab2 <- prune_vocabulary(vocab2,term_count_min = 4)

pruned_vocab3 <- prune_vocabulary(vocab3,term_count_min = 2)

pruned_vocab4 <- prune_vocabulary(vocab4,term_count_min = 2)

proc.time()-ptm
####################################
freqr <- pruned_vocab[1]
freqr <- data.frame(freqr)
freqr <- freqr[, 1:2]
freqr <- freqr %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)
top30 <- head(freqr, 30)
bottom30 <- tail(freqr, 30)

p <- ggplot(data = top30, aes(x = reorder(Word, -Frequency), y = Frequency))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1), axis.title.x = element_blank()) + labs(title = "Top 30 Unigrams")
p

freqr4 <- pruned_vocab4[1]
freqr4 <- data.frame(freqr4)
freqr4 <- freqr4[, 1:2]
freqr4 <- freqr4 %>% arrange(desc(vocab.terms_counts)) %>%
  rename(word = vocab.terms, frequency = vocab.terms_counts)
tri_top30 <- head(freqr4, 30)

spl <- strsplit(freqr4$word,"_")
w1 <- sapply(spl,"[[",1)
w2 <- sapply(spl,"[[",2)
w3 <- sapply(spl,"[[",3)
w4 <- sapply(spl,"[[",4)
quad_df <- data.frame(w1,w2,w3,w4,freqr4$frequency)
