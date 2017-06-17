preprocess_input <- function(str,profanities){
  graphToSpace <- function(x){x <- gsub("[^[:graph:]]", " ", x)}
  # apply lower case transformation and stemmer to all the text data
  str <- str %>%
    graphToSpace %>%
    tolower %>% 
    removeConfounders %>%
    removePunctuation %>%
    removeNumbers %>%
    removeWords(profanities) %>%
    removeWords(letters[!letters %in% c("a","i")]) %>%
    stripWhitespace
  str
}

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

get_wordcount <- function(str){
  wordcount <- length(unlist(strsplit(str," ")))
  wordcount
}
predict_ecta_new <- function(str,ecta_df){
  if(get_wordcount(str)>=5){
    new_str <- paste(tail(unlist(strsplit(str," ")),5))
    ret_val <-  as.character(ecta_df[(ecta_df$w1==new_str[1] & ecta_df$w2==new_str[2] & 
                                        ecta_df$w3==new_str[3] & ecta_df$w4==new_str[4] & ecta_df$w5==new_str[5]),][,6])
    ret_val[1:3]
  } else {NA}
}
predict_penta_new <- function(str,penta_df){
  if(get_wordcount(str)>=4){
    new_str <- paste(tail(unlist(strsplit(str," ")),4))
    ret_val <- as.character(penta_df[(penta_df$w1==new_str[1] & penta_df$w2==new_str[2] & 
                                        penta_df$w3==new_str[3] & penta_df$w4==new_str[4]),][,5])
    ret_val[1:3]
  } else {NA}
}
predict_quad_new <- function(str,quad_df){
  if(get_wordcount(str)>=3){
    new_str <- paste(tail(unlist(strsplit(str," ")),3))
    ret_val <- as.character(quad_df[(quad_df$w1==new_str[1] & quad_df$w2==new_str[2] & quad_df$w3==new_str[3]),][,4])
    ret_val[1:3]
  } else {NA}
}
predict_tri_new <- function(str,tri_df){
  if(get_wordcount(str)>=2){
    new_str <- paste(tail(unlist(strsplit(str," ")),2))
    ret_val <- as.character(tri_df[(tri_df$w1==new_str[1] & tri_df$w2==new_str[2]),][,3])
    ret_val[1:3]
  } else {NA}
}
predict_bi_new <- function(str,bi_df){
  if(get_wordcount(str)>=1){
    new_str <- paste(tail(unlist(strsplit(str," ")),1))
    ret_val <- as.character(bi_df[bi_df$w1==new_str[1],][,2])
    ret_val[1:3]
  } else {NA}
}

sbo_predict <- function(str,uni_df,bi_df,tri_df,quad_df,penta_df,ecta_df,profanities){
  preprocess_input(str,profanities)
  predictions <- character(0)
  predictions <- c(predictions,predict_ecta_new(str,ecta_df))
  predictions <- c(predictions,predict_penta_new(str,penta_df))
  predictions <- c(predictions,predict_quad_new(str,quad_df))
  predictions <- c(predictions,predict_tri_new(str,tri_df))
  predictions <- c(predictions,predict_bi_new(str,bi_df))
  predictions <- c(predictions,uni_df[,1][1:3])
  predictions <- unique(predictions)
  predictions[!is.na(predictions)][1:3]
}
