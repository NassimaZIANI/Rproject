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
shinyUI(fluidPage(

    # Application title
    titlePanel("Physics Toolbox Project"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dataset",label = "Choose a dataset : ",choices = c("omov", "xmov", "walkmov")),
            sliderInput("meanthresh", 
                        "Left rolling average threeshold", 
                        min = 0,
                        max = 1, 
                        value = 0.5),
            sliderInput("sdthresh", 
                        "Left rolling standard deviation threeshold", 
                        min = 0,
                        max = 1, 
                        value = 0.2),
            sliderInput("timethresh", 
                        "Time threeshold", 
                        min = 0,
                        max = 1, 
                        value = 0.2),
            numericInput("obs", "Number of observations to view:", 5),
            
            textInput("label", "Chose a label", "movement"),
            
            downloadButton("downloadData", "Download")
        ),
        mainPanel(
            h3(textOutput("title")), 
            tableOutput("view"),
            plotOutput("graph",height="350px")
        )
    )
)
)
