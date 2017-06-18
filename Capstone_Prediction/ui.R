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
               p("To build the predictive model, 1.000.000 lines from all twitter, blogs and news datasets were sampled. 
                  The sample dataset was then cleaned, by removing all non-ascii characters, like emoji, being converted 
                  to lowercase letters and then by removing all contractions, punctuation, numbers, profanities, leftout 
                  letters and extra whitespaces."),
               br(),
               h4("Prediction Model"),
               p("The data was then tokenized to form Maximum Likelihood Estimation (MLE) matrices of various n-grams. 
                  For the sake of accuracy, all frequencies up to 6-grams were computed."),
               br(),
               p("Finally, the top 3 predictions, using a simple back-off model, are being calculated as predictions 
                  to the user input. The reason for having 3 predictions instead of 1 is that the accuracy the user 
                  experiences is substantially increased.")
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
               
               a(href="https://github.com/SiGm4/Data-Science-Capstone-Project/",
                 img(src="http://dimgkiokas.com/github-icon.png",alt="github-icon",height="36px",width="36px")),
               
               a(href="http://www.dimgkiokas.com",
                 img(src="http://dimgkiokas.com/web-icon.png",alt="web-icon",height="36px",width="36px"))
               
             )    
         )
      )
)
