#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(navbarPage("Data Science Capstone Project", theme="main.css",
                   tabPanel("Prediction",
                     sidebarLayout(
                       sidebarPanel(
                          textInput("string_input","Please enter text here:",placeholder="Your text goes here")
                       ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(
                         h4("Next predicted word:"),
                         textOutput("prediction")
                       )
                      )
                   ),
                   tabPanel("Algorithm"),
                   tabPanel("About")
))



# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Data Science Capstone Project - Dimitris Gkiokas"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        textInput("string_input","Please enter text here:",placeholder="Your text goes here")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       textOutput("prediction")
#     )
#   )
# ))
