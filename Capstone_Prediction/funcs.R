preprocess_input <- function(str){
  toSpace <- function(x){x <- gsub("[^[:graph:]]", " ", x)}
  # apply lower case transformation and stemmer to all the text data
  str <- str %>%
    toSpace %>%
    removeConfounders %>%
    removePunctuation %>%
    tolower %>% 
    removeNumbers %>%
    stripWhitespace
  str
}