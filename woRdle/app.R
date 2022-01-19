#
# This is a Shiny web application for playing Wordle
#
#
# https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
#

library(shiny)
load("~/Documents/RProjects/woRdle/words.Rdata")
set.seed(as.numeric(Sys.Date()))
# sets a new seed each day, so the word changes each day
word <- sample(mystery_words,1)
# selects a random word as the word for the day

# Define UI for application that draws a histogram
ui <- fluidPage(

  # first set of 5 boxes for guess entry
  textInput("g1","First Guess:"),
  actionButton("click","Guess!")
  # dynamically add a row of boxes each time they submit a guess
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # error checking
  # check for guess in guessable list
  # this handles anything that isn't 5 characters as well
  
  
  # check for duplicate guesses (same word as previous)
  
  

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
