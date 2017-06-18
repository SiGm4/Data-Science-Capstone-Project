library(shiny)
library(dplyr)
library(tm)

shinyServer(function(input, output,session) {
  uni_df <- readRDS("./ngrams/unigrams.Rdata")
  bi_df <- readRDS("./ngrams/bigrams.Rdata")
  tri_df <- readRDS("./ngrams/trigrams.Rdata")
  quad_df <- readRDS("./ngrams/quadgrams.Rdata")
  penta_df <- readRDS("./ngrams/pentagrams.Rdata")
  ecta_df <- readRDS("./ngrams/ectagrams.Rdata")
  profanities <-  readLines('bad-words.txt')
  source("funcs.R")
  
  theprediction <- reactive({sbo_predict(input$string_input,uni_df,bi_df,tri_df,quad_df,penta_df,ecta_df,profanities)})
  output$best_prediction <- renderText({
    theprediction()[1]
  })
  output$predButton1 <- renderUI({
    actionButton("predButtona1", label = theprediction()[1])
  })
  output$predButton2 <- renderUI({
    actionButton("predButtona2", label = theprediction()[2])
  })
  output$predButton3 <- renderUI({
    actionButton("predButtona3", label = theprediction()[3])
  })
  observeEvent(input$predButtona1,{
    if (input$string_input!=""){
      new_input <- paste(input$string_input,theprediction()[1])
    }
    else{
      new_input <- theprediction()[1]
    }
    updateTextInput(session, "string_input", value = new_input)
  })
  observeEvent(input$predButtona2,{
    if (input$string_input!=""){
      new_input <- paste(input$string_input,theprediction()[2])
    }
    else{
      new_input <- theprediction()[2]
    }
    updateTextInput(session, "string_input", value = new_input)
  })
  observeEvent(input$predButtona3,{
    if (input$string_input!=""){
      new_input <- paste(input$string_input,theprediction()[3])
    }
    else{
      new_input <- theprediction()[3]
    }
    updateTextInput(session, "string_input", value = new_input)
  })
  
})
