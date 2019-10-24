library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(zoo)
library(textstem)
library(wesanderson)
library(wordcloud)
library(patchwork)

shinyServer(function(input, output, session){
  
  # --------- Functions for the server ------------
  
  whats_rd <- function (x, sender1, sender2) {
    my_stops <- tibble(word = c("omit", "medium", "https"),
                       lexicon = c("my_stops", "my_stops", "my_stops"))
    my_stops<-rbind(stop_words, my_stops)
    
    sender1 <- enquo(sender1)
    sender2 <- enquo(sender2)
    
    read_lines(x) %>% 
      tibble() %>% 
      rename(msg_text = ".") %>% 
      mutate (msg_text = parse_character(msg_text)) %>% 
      mutate (msg_stamp = str_extract(msg_text, "\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d\\,\\ \\d\\d\\:\\d\\d\\ \\-\\ [^0-9]+\\: ")) %>% 
      separate(msg_stamp, into = c("date", "sender"), sep = " - ") %>% 
      mutate (msg_text = str_remove(msg_text, "\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d\\,\\ \\d\\d\\:\\d\\d\\ \\-\\ [^0-9]+\\: ")) %>% 
      mutate (sender = na.locf(sender, na.rm = FALSE)) %>% 
      mutate (date = na.locf(date, na.rm = FALSE)) %>% 
      filter(!is.na(msg_text),
             !is.na(date)) %>% 
      separate(date, into = c("date", "time"), sep =",") %>% 
      mutate (date = lubridate::dmy(date)) %>% 
      mutate (shortsend = case_when (str_detect(sender, !!sender1) ~ !!sender1,
                                     str_detect(sender, !!sender2) ~ !!sender2)) %>% 
      unnest_tokens(word, msg_text) %>% 
      mutate (word = (lemmatize_strings(word))) %>% 
      anti_join(my_stops) %>% 
      filter(str_detect(word, '[^0-9]'))
    
  }
  
  # ---------- Importing Data -----------------
  
  dat <- reactive ({
    
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)
    }
    name1 <- input$name1
    if (is.null(name1)) {
      return(NULL)
    }
    name2 <- input$name2
    if (is.null(name2)) {
      return(NULL)
    }
    
    my_stops <- tibble(word = c("omit", "medium", "https"),
                       lexicon = c("my_stops", "my_stops", "my_stops"))
    my_stops<-rbind(stop_words, my_stops)
    
    read_lines(infile$datapath) %>% 
      tibble() %>% 
      rename(msg_text = ".") %>% 
      mutate (msg_text = parse_character(msg_text)) %>% 
      mutate (msg_stamp = str_extract(msg_text, "\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d\\,\\ \\d\\d\\:\\d\\d\\ \\-\\ [^0-9]+\\: ")) %>% 
      separate(msg_stamp, into = c("date", "sender"), sep = " - ") %>% 
      mutate (msg_text = str_remove(msg_text, "\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d\\,\\ \\d\\d\\:\\d\\d\\ \\-\\ [^0-9]+\\: ")) %>% 
      mutate (sender = na.locf(sender, na.rm = FALSE)) %>% 
      mutate (date = na.locf(date, na.rm = FALSE)) %>% 
      filter(!is.na(msg_text),
             !is.na(date)) %>% 
      separate(date, into = c("date", "time"), sep =",") %>% 
      mutate (date = lubridate::dmy(date)) %>% 
      mutate (shortsend = case_when (str_detect(sender, !!name1) ~ !!name1,
                                     str_detect(sender, !!name2) ~ !!name2)) %>% 
      unnest_tokens(word, msg_text) %>% 
      mutate (word = (lemmatize_strings(word))) %>% 
      anti_join(my_stops) %>% 
      filter(str_detect(word, '[^0-9]'))
    
  })
  
  # ---------- Output raw data --------------
  
  output$raw_data <- renderDT({
    if (!is.null(dat())) {
      dat()
    }
  })
  
  
})