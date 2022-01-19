#library(googlesheets4)
library(tidyverse)
mystery_words <- googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1-M0RIVVZqbeh0mZacdAsJyBrLuEmhKUhNaVAI-7pr2Y/edit#gid=0",col_names = F) %>% pull() %>% toupper()

guessable_words <- googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1KR5lsyI60J1Ek6YgJRU2hKsk4iAOWvlPLUWjAZ6m8sg/edit#gid=0",col_names = F) %>% pull() %>% toupper()
# https://docs.google.com/spreadsheets/d/1KR5lsyI60J1Ek6YgJRU2hKsk4iAOWvlPLUWjAZ6m8sg/edit#gid=0

save(mystery_words,guessable_words,file = "words.Rdata")
