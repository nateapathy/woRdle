#
# This is a Shiny web application for playing Wordle
# https://www.r-bloggers.com/2022/01/playing-wordle-in-r/
#
library(shiny)
library(tidyverse)
load("~/Documents/RProjects/woRdle/words.Rdata")
set.seed(as.numeric(Sys.Date()))
# sets a new seed each day, so the word changes each day
word <- sample(mystery_words,1)
ltrs <- strsplit(word,"")[[1]]
word_df <- tibble(gno=0,
                  g=word,
                  lno=c(1:5),
                  ltr=ltrs,
                  cor=TRUE)

# Define UI for application
ui <- fluidPage(align="center",
  textInput("guess_in","Your Guess:"),
  actionButton("guess","Guess!"),
  textOutput("guess_out"),
  # dynamically add a row of boxes each time they submit a guess
  plotOutput("resplot",width = "400px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  all_guesses <- eventReactive(input$guess==1,{
    as.character()
  })
  all_guesses <- eventReactive(input$guess>1,{
      append(all_guesses,toupper(input$guess_in))
  })
  
  guesscheck <- eventReactive(input$guess,{
    # when they hit guess, store the guess
    g <- toupper(input$guess_in)
    # then check if its legit
    validate(need(g %in% guessable_words,
                  "Oops! That's not in the list of guessable words! (or you already guessed that)")
    )
    g
  })
  
  output$guess_out <- renderText({
    guesscheck()
  })
  
  guessdata <- eventReactive(input$guess,{
    tibble(gno=0,
           g=word,
           lno=c(1:5),
           ltr=NA,
           cor=NA,
           acc=2) %>%
      arrange(gno,lno)
  })
  
  output$resplot <- renderPlot({
    guessdata() %>%
      #filter(gno!=0) %>%
      mutate(gord=factor(gno,levels = rev(unique(gno)))) %>%
      ggplot(aes(x=lno,y=gord,fill=factor(acc),label=ltr)) +
      geom_tile(color="black",size=2) +
      geom_text(size=10) +
      scale_fill_manual(values=c("grey","yellow","green")) +
      theme_void() +
      theme(legend.position = "none")
    })
  # check for max guesses (n=6), do not eval if they've already guessed 6 valid guesses
}

# Run the application 
shinyApp(ui = ui, server = server)
