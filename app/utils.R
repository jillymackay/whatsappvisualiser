# utils for app

library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(zoo)
library(textstem)
library(wesanderson)
library(wordcloud)
library(patchwork)


whatplot_date <- function (data)  {
  data %>% 
    ggplot (aes (x = date, fill = shortsend)) +
    geom_bar (stat = "count", position = "dodge2") +
    theme_classic() +
    scale_fill_manual(values = wes_palette("Moonrise3")) +
    theme_classic() +
    theme(legend.position = "none") +
    labs (x = "Date", y= "n messages", title = "WhatsApp message history for 2 person chat") +
    facet_grid(rows = vars(shortsend))
  
}



whatplot_hours <- function (data) {
  data %>% 
    separate (time, into = c("hours", "minutes"), sep =":") %>% 
    ggplot (aes (x = hours, fill = hours)) +
    geom_bar (stat = "count") +
    scale_fill_manual(values = timepal) +
    theme_classic() +
    theme(legend.position = "none") +
    labs (x = "Hour", y = "n messages")
  
}


whatplot_compare <- function (data) {
  data %>%
    filter(str_detect(word, '[^0-9]')) %>% 
    count (word, shortsend, sort = TRUE) %>%
    reshape2::acast(word ~ shortsend, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#533366",    "#E9967A"), max.words = 150, 
                     rot.per = 0, use.r.layout = FALSE)
  
}




whatplot_words <- function (data, sender1, sender2) {
  sender1 <- enquo(sender1)
  sender2 <- enquo(sender2)
  
  p1 <- data %>% 
    filter(shortsend == !!sender1) %>% 
    count(word, shortsend, sort = TRUE)  %>% 
    head(10) %>% 
    ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
    geom_bar(stat = "identity", position = "dodge2") +
    scale_fill_manual(values = timepal) +
    labs (x = !!sender1, y = NULL) +
    theme_classic() +
    theme(legend.position = "none") 
  
  
  p2 <-words %>% 
    filter(shortsend == !!sender2) %>% 
    count(word, shortsend, sort = TRUE)  %>% 
    head(10) %>% 
    ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
    scale_fill_manual(values = timepal) +
    geom_bar(stat = "identity", position = "dodge2") +
    labs (x = !!sender2, y = NULL) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_continuous(position = "right") 
  
  p1 + p2
}
