toCorpus <- function(sampleData){
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
  corpus
}

unigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}