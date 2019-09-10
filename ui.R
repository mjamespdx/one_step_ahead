#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        
            "One Step Ahead",
            
            tabPanel(
                     "The app.",

                     tags$style(type = "text/css", "#userOut td { border: none; }"),
                     
                     fluidRow(align = 'center',
                     column(width = 3, offset = 4, 
                            textInput("userIn", label = NA, value = "Give it a"),
                     h3(tableOutput("userOut"))
                     ))),
            
            tabPanel("What is this thing?",
                     column(width = 6, offset = 3,
                     
                     helpText("One Step Ahead is a text prediction application—it 
                              can read your mind!"),
                     p(), 
                     helpText("Just kidding, it uses the smartly titled Stupid Backoff method 
                              to guess what you're going to say."),
                     em(h4("How does it work?")),
                     helpText("Start typing in the little box! The program will give you some 
                              options as to what it thinks you will say next. Pretty cool, huh?"),
                     em(h4("What are those numbers?")),
                     p(),
                     helpText("Those are probabilities, my friend! Each word is determined to 
                              be that likely to be the next in your sentence."),
                     em(h4("They seem kind of low...")),
                     helpText("Indeed they do. There are many, many words in the 
                              English language, and language is fluid. We can only provide 
                              educated suggestions. Only you know what you're going to say for sure!"),
                     em(h4("Huh. Technology is pretty neat.")),
                     helpText("It sure is!"),
                     em("© M. Sieviec, 2018")
            ))
    )
)

