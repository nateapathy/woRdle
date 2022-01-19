library(tidyverse)
load("words.Rdata")

word <- "BYLAW"
ltrs <- strsplit(word,"")[[1]]
word_df <- tibble(gno=0,
                  g=word,
                  lno=c(1:5),
                  ltr=ltrs,
                  cor=TRUE)
word_df

g1 <- toupper("boats")
g1eval <- strsplit(g1,"")[[1]] == ltrs

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
                       TRUE~0)) -> gdf

g2 <- toupper("bloat")
g2eval <- strsplit(g2,"")[[1]] == ltrs

gdf %>%
  bind_rows(tibble(gno=2,
                   g=g2,
                   lno=c(1:5),
                   ltr=strsplit(g2,"")[[1]],
                   cor=g2eval)) %>%
  mutate(inword=case_when(ltr %in% ltrs~TRUE,
                          TRUE~FALSE),
         acc=case_when(cor==T~2,
                       cor==F&inword==T~1,
                       TRUE~0)) -> gdf

gdf %>%
  filter(gno!=0) %>%
  mutate(gord=factor(gno,levels = rev(unique(gno)))) %>%
  ggplot(aes(x=lno,y=gord,fill=factor(acc),label=ltr)) +
  geom_tile(color="black",size=2) +
  geom_text(size=10) +
  scale_fill_manual(values=c("grey","yellow","green")) +
  theme_void() +
  theme(legend.position = "none")
