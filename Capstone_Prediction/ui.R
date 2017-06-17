#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
js_focus_back_on_input <- 
  '
$(document).ready(function() {
$("#predButton1,#predButton2,#predButton3").click(function(e) {
$("#string_input").focus();
});
});
'

shinyUI(navbarPage("Data Science Capstone Project", theme="main.css",
         tabPanel("Prediction",
             tags$script(js_focus_back_on_input),      
             mainPanel(
                h2("Word Prediction:"),
                textInput("string_input","",placeholder="Enter text here")
             ),
             mainPanel(
               h4("Predicted words:"),
               div(id="predButtons",
                 uiOutput("predButton1"),
                 uiOutput("predButton2"),
                 uiOutput("predButton3")
               ),
               div(id="top_prediction",
                h4("Top prediction:"),
                textOutput("best_prediction")
               )
             )
         ),
         tabPanel("Algorithm",
             mainPanel(
               h2("Algorithm"),
               h4("Data Cleaning"),
               p("First of all, all the non letter characters, like emojis, are being removed. 
                 Then, all confounders, punctuation and numbers are removed, 
                 and the cleaning is finished by removing any extra whitespaces."),
               br(),
               h4("Prediction Model"),
               p("In order to predict the consequent word(s), a stupid back-off model is being used. It starts from
                 the 6-gram, and backs-off to the unigrams if no solution is found in between.")
             )
         ),
         tabPanel("About",
             mainPanel(
               h2("About this project"),
               p("This project was completed during the Capstone Project of the Data Science Coursera Specialization,
                 offered by Johns Hopkins University."),
               p("The instructors for this project were Brian Caffo, Roger D. Peng and Jeff Leek."),
               p("This project is a collaboration with SwiftKey, and its goal is the development of a 
                 text prediction algorithm, predicting the next word from a sentence.")
             ),    
             mainPanel(
               h2("About me"),
               p("My name is Dimitris Gkiokas and I am from Greece. Find me below:"),
               a(href="https://www.facebook.com/dimitris.gkiokas.77",
                 img(src="http://dimgkiokas.com/fb-icon.png",alt="fb-icon",height="36px",width="36px")),
               
               a(href="https://twitter.com/dimgkiokas",
                 img(src="http://dimgkiokas.com/twitter-icon.png",alt="tw-icon",height="36px",width="36px")),
               
               a(href="https://www.linkedin.com/in/dimitris-gkiokas/",
                 img(src="http://dimgkiokas.com/linkedin-icon.png",alt="linkedin-icon",height="36px",width="36px")),
               
               a(href="http://www.dimgkiokas.com",
                 img(src="http://dimgkiokas.com/web-icon.png",alt="web-icon",height="36px",width="36px"))
               
             )    
         )
      )
)
