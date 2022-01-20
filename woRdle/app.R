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
# selects a random word as the word for the day
base_df <- tibble(gno=1,
                  g="XXXXX",
                  lno=c(1:5),
                  ltr=NA,
                  cor=NA,
                  acc=0) %>%
  arrange(gno,lno)

base_plot <- base_df %>%
  filter(gno!=0) %>%
  mutate(gord=factor(gno,levels = rev(unique(gno)))) %>%
  ggplot(aes(x=lno,y=gord,fill=factor(acc),label=ltr)) +
  geom_tile(color="black",size=2) +
  geom_text(size=10) +
  scale_fill_manual(values=c("grey","yellow","green")) +
  theme_void() +
  theme(legend.position = "none")

gnum <- 0 # initialize guess number

# Define UI for application
ui <- fluidPage(align="center",
  textInput("g","Your Guess:"),
  actionButton("guess","Guess!"),
  textOutput("err"),
  # dynamically add a row of boxes each time they submit a guess
  plotOutput("resplot",width = "400px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #g <- observeEvent(input$guess,input$g)
  #prev_guesses <- as.character(g)
  #append(prev_guesses,g)
  #print(prev_guesses)
  #gnum <- eventReactive({input$guess})
  
  output$err <- eventReactive(input$guess,
    {
      validate(need(toupper(input$g) %in% guessable_words,
               "Oops! That's not in the list of guessable words!")
               )
      validate(need(!toupper(input$g) %in% prev_guesses,
                    "Looks like you already guessed that word!")
               )
      toupper(input$g)
    }
  )
  
  output$resplot <- renderPlot({
    if (gnum==0) { 
      {
        base_plot
        }
    }
    if (gnum==1) {
      
    }
    if (gnum==2) {
      
    }
    if (gnum==3) {
      
    }
    if (gnum==4) {
      
    }
    if (gnum==5) {
      
    }
    if (gnum==6) {
      
    }
    if (gnum>6) {
      base_plot +
        geom_text(aes(x=3,y=3,label="Sorry! No more guesses!"))
    }
    })

  # check for max guesses (n=6), do not eval if they've already guessed 6 valid guesses

}

# Run the application 
shinyApp(ui = ui, server = server)
