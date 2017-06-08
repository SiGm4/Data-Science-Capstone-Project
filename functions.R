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
############################################
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
get_wordcount <- function(str){
  wordcount <- length(unlist(strsplit(str," ")))
  wordcount
}
predict_ecta_new <- function(str){
  if(get_wordcount(str)>=5){
    new_str <- paste(tail(unlist(strsplit(str," ")),5))
    as.character(ecta_df[(ecta_df$w1==new_str[1] & ecta_df$w2==new_str[2] & 
               ecta_df$w3==new_str[3] & ecta_df$w4==new_str[4] & ecta_df$w5==new_str[5]),][,6])
  } else {NA}
}
predict_penta_new <- function(str){
  if(get_wordcount(str)>=4){
    new_str <- paste(tail(unlist(strsplit(str," ")),4))
    as.character(penta_df[(penta_df$w1==new_str[1] & penta_df$w2==new_str[2] & 
                penta_df$w3==new_str[3] & penta_df$w4==new_str[4]),][,5])
  } else {NA}
}
predict_quad_new <- function(str){
  if(get_wordcount(str)>=3){
    new_str <- paste(tail(unlist(strsplit(str," ")),3))
    as.character(quad_df[(quad_df$w1==new_str[1] & quad_df$w2==new_str[2] & quad_df$w3==new_str[3]),][,4])
  } else {NA}
}
predict_tri_new <- function(str){
  if(get_wordcount(str)>=2){
    new_str <- paste(tail(unlist(strsplit(str," ")),2))
    as.character(tri_df[(tri_df$w1==new_str[1] & tri_df$w2==new_str[2]),][,3])
  } else {NA}
}
predict_bi_new <- function(str){
  if(get_wordcount(str)>=1){
    new_str <- paste(tail(unlist(strsplit(str," ")),1))
    as.character(bi_df[bi_df$w1==new_str[1],][,2])
  } else {NA}
}
predict_new <- function(str){
  preprocess_input(str)
  pred6 <- predict_ecta_new(str)
  if (is.na(pred6)){
    pred5 <- predict_penta_new(str)
    if (is.na(pred5)){
      pred4 <- predict_quad_new(str)
      if (is.na(pred4)){
        pred3 <- predict_tri_new(str)
        if (is.na(pred3)){
          pred2 <- predict_bi_new(str)
          if (is.na(pred2)){
            freqr[1,][[1]]
          } else{pred2}
        } else{pred3}
      } else{pred4}
    } else{pred5}
  } else{pred6}
}

predict_new_tester <- function(str){
  preprocess_input(str)
  pred6 <- predict_ecta_new(str)
  if (length(pred6)==0 || is.na(pred6)){
    pred5 <- predict_penta_new(str)
    if (length(pred5)==0 || is.na(pred5)){
      pred4 <- predict_quad_new(str)
      if (length(pred4)==0 || is.na(pred4)){
        pred3 <- predict_tri_new(str)
        if (length(pred3)==0 || is.na(pred3)){
          #pred2 <- predict_bi_new(str)
          #if (is.na(pred2)){
          freqr[1,][[1]]
          #} else{pred2}
        } else{pred3}
      } else{pred4}
    } else{pred5}
  } else{pred6}
}

predict_bi_new("Hello my dear friend how are you")
predict_tri_new("Hello my dear friend how are you")
predict_quad_new("Hello my dear friend how are you")
predict_penta_new("Hello my dear friend how are you")
predict_ecta_new("Hello my dear friend how are you")
predict_new("Hello my dear friend how are you")
predict_bi_new("a case of")
predict_tri_new("a case of")
predict_quad_new("a case of")
predict_penta_new("and a case of")
predict_ecta_new("bouquet and a case of")
predict_new("buy a case of")

predict_new("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
predict_new("You're the reason why I smile everyday. Can you follow me please? It would mean the")
predict_new("Hey sunshine, can you follow me and make me the")
predict_new("Very early observations on the Bills game: Offense still struggling but the")
predict_new("Go on a romantic date at the")
predict_new("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
predict_new("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
predict_new("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
predict_new("Be grateful for the good times and keep the faith during the")
predict_new("If this isn't the cutest thing you've ever seen, then you must be")

preprocess_input("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
preprocess_input("You're the reason why I smile everyday. Can you follow me please? It would mean the")
preprocess_input("Hey sunshine, can you follow me and make me the")
preprocess_input("Very early observations on the Bills game: Offense still struggling but the")
preprocess_input("Go on a romantic date at the")
preprocess_input("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
preprocess_input("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
preprocess_input("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
preprocess_input("Be grateful for the good times and keep the faith during the")
preprocess_input("If this isn't the cutest thing you've ever seen, then you must be")

preprocess_input("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")
preprocess_input("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his")
preprocess_input("I'd give anything to see arctic monkeys this")
preprocess_input("Talking to your mom has the same effect as a hug and helps reduce your")
preprocess_input("When you were in Holland you were like 1 inch away from me but you hadn't time to take a")
preprocess_input("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the")
preprocess_input("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
preprocess_input("Every inch of you is perfect from the bottom to the")
preprocess_input("I'm thankful my childhood was filled with imagination and bruises from playing")
preprocess_input("I like how the same people are in almost all of Adam Sandler's")
