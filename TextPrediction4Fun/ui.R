#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
fluidPage(
  titlePanel("Hello World!!"),
             h2("This is my cool word prediction widget!!"),
  fluidRow(
    column(5, wellPanel(
           textInput("textvar", "Please type any word, this app will attempt to guess the next one", "happy "),
      
      br(),
      h5("1. Enter lowercase text and the app will predict current word as you type"), 
      h5("2. When you hit the space bar, the app will try to predict the next word.")
      
      
      # ,actionButton("goButton", "Go!")
    )),
    column(5,wellPanel(
           h4("Possible current word or next word"), textOutput("summary") 
           ) 
    
           )
  )
)