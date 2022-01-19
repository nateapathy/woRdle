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

# Define UI for application
ui <- fluidPage(
  textInput("g","Your Guess:"),
  actionButton("guess","Guess!"),
  # dynamically add a row of boxes each time they submit a guess
  plotOutput("resplot",width = "400px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  prev_guesses <- as.character()
  
  g1 <- eventReactive(input$guess,
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

  g1eval <- eventReactive(strsplit(g1,"")[[1]] == ltrs)
  
  gdf <- eventReactive(input$guess, {
    word_df %>%
      bind_rows(tibble(gno=1,
                       g=g1,
                       lno=c(1:5),
                       ltr=strsplit(g1,"")[[1]],
                       cor=g1eval)) %>%
      mutate(inword=case_when(ltr %in% ltrs~TRUE,
                              TRUE~FALSE),
             acc=case_when(cor==T~2,
                           cor==F&inword==T~1,
                           TRUE~0))
    }
    )

  
  # check for max guesses (n=6), do not eval if they've already guessed 6 valid guesses

    output$resplot <- renderPlot({
      gdf %>%
        filter(gno!=0) %>%
        mutate(gord=factor(gno,levels = rev(unique(gno)))) %>%
        ggplot(aes(x=lno,y=gord,fill=factor(acc),label=ltr)) +
        geom_tile(color="black",size=2) +
        geom_text(size=10) +
        scale_fill_manual(values=c("grey","yellow","green")) +
        theme_void() +
        theme(legend.position = "none") 
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
