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
gdf <- tibble(g="AAAAA",
              lno=c(1:5),
              ltr=ltrs,
              cor=TRUE,
              orig=TRUE)

# Define UI for application
ui <- fluidPage(align="center",
  textInput("guess_in","Guess:"),
  actionButton("guess","Guess!"),
  textOutput("guess_out"),
  # dynamically add a row of boxes each time they submit a guess
  plotOutput("resplot",width = "400px",height = "100px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
    g <- str_split(toupper(input$guess_in),"")[[1]]
    #right <- input$guess_in==word # check if it's right; technically don't even need this
    tibble(l=g,
           ltrs=g %in% ltrs, # check if letters are in word
           spots=g==ltrs) %>% # check if letters are in the right place
      mutate(color=case_when(spots==T~"green",
                             spots==F&ltrs==T~"yellow",
                             spots==F&ltrs==F~"grey"),
             lno=c(1:5)) -> gdat
    gdat
    # only update data if it's a new legit guess
  })
  
  output$resplot <- renderPlot({
    guessdata() %>%
      ggplot(aes(x=lno,y=1,fill=color,label=l)) +
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
