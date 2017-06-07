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
  corpus <- tm_map(corpus,removeWords,letters[!letters %in% c("a","i")])
  #Striping all extra whitespace
  corpus <- tm_map(corpus,stripWhitespace)
  corpus
}

unigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 1))}
BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}
QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}

predict_bi <- function(str,options){
  input <- unlist(strsplit(str," "))
  as.character(bi_df[(bi_df$w1==input[1] & bi_df$w2 %in% options),][,2])
}
predict_tri <- function(str,options){
  input <- unlist(strsplit(str," "))
  as.character(tri_df[(tri_df$w1==input[1] & tri_df$w2==input[2] & tri_df$w3 %in% options),][,3])
}
predict_quad <- function(str,options){
  input <- unlist(strsplit(str," "))
  as.character(quad_df[(quad_df$w1==input[1] & quad_df$w2==input[2] & quad_df$w3==input[3] & quad_df$w4 %in% options),][,4])
}

predict_tri2 <- function(str,options){
  input <- unlist(strsplit(str," "))
  as.character(tri_df[(tri_df$w1==input[1] & tri_df$w2==input[2]),][,3])
}
predict_quad2 <- function(str,options){
  input <- unlist(strsplit(str," "))
  as.character(quad_df[(quad_df$w1==input[1] & quad_df$w2==input[2] & quad_df$w3==input[3]),][,4])
}

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
    result <- c(predict_bi(str),predict_tri2(new_str))
  }
  else if (wordcount >= 3){
    new_str1 <- tail(unlist(strsplit(str," ")),1)
    new_str2 <- paste(tail(unlist(strsplit(str," ")),2),collapse=" ")
    new_str3 <- paste(tail(unlist(strsplit(str," ")),3),collapse=" ")
    print(new_str1)
    print(new_str2)
    print(new_str3)
    result <- c(predict_bi(new_str1),predict_tri2(new_str2),predict_quad2(new_str3))
  }
  result
}