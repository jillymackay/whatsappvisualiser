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
  
  timepal <- wes_palette("Moonrise3", 24, type = "continuous")
  
  
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
  
  # ---------- Create plots ----------------
  
  
  output$whatplot_time <- renderPlot(
    {
      p1 <- dat() %>% 
        ggplot (aes (x = date, fill = shortsend)) +
        geom_bar (stat = "count", position = "dodge2") +
        theme_classic() +
        scale_fill_manual(values = wes_palette("Moonrise3")) +
        theme_classic() +
        theme(legend.position = "none") +
        labs (x = "Date", y= "n messages", title = "WhatsApp message history for 2 person chat") +
        facet_grid(rows = vars(shortsend))
      
      p2 <- dat() %>% 
        separate (time, into = c("hours", "minutes"), sep =":") %>% 
        ggplot (aes (x = hours, fill = hours)) +
        geom_bar (stat = "count") +
        scale_fill_manual(values = timepal) +
        theme_classic() +
        theme(legend.position = "none") +
        labs (x = "Hour", y = "n messages")
      
      p1 + p2 + plot_layout(ncol = 1, heights = c(1, 1))
      
    }
    
    
  )
  
  output$whatplot_date <- renderPlot(
    {
      p1 <- dat() %>% 
        ggplot (aes (x = date, fill = shortsend)) +
        geom_bar (stat = "count", position = "dodge2") +
        theme_classic() +
        scale_fill_manual(values = wes_palette("Moonrise3")) +
        theme_classic() +
        theme(legend.position = "none") +
        labs (x = "Date", y= "n messages", title = "WhatsApp message history for 2 person chat") +
        facet_grid(rows = vars(shortsend))
      p
    }
  )
  
  output$whatplot_hours <- renderPlot(
    {
      p <- dat() %>% 
        separate (time, into = c("hours", "minutes"), sep =":") %>% 
        ggplot (aes (x = hours, fill = hours)) +
        geom_bar (stat = "count") +
        scale_fill_manual(values = timepal) +
        theme_classic() +
        theme(legend.position = "none") +
        labs (x = "Hour", y = "n messages")
      
      p
    }
  )
  
  
 output$whatplot_compare <- renderPlot (
   {
     p <- dat() %>% 
       filter(str_detect(word, '[^0-9]')) %>% 
       count (word, shortsend, sort = TRUE) %>%
       reshape2::acast(word ~ shortsend, value.var = "n", fill = 0) %>%
       comparison.cloud(colors = c("#533366",    "#E9967A"), max.words = 150, 
                        rot.per = 0, use.r.layout = FALSE)
       
   
   }
 )
  
  
  output$whatplot_words <- renderPlot(
    {
      
      p1 <- dat() %>% 
        filter(shortsend == (input$name1)) %>% 
        count(word, shortsend, sort = TRUE)  %>% 
        head(10) %>% 
        ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
        geom_bar(stat = "identity", position = "dodge2") +
        scale_fill_manual(values = timepal) +
        labs (x = (input$name1), y = NULL) +
        theme_classic() +
        theme(legend.position = "none") 
      
      
      p2 <-dat() %>% 
        filter(shortsend ==(input$name2)) %>% 
        count(word, shortsend, sort = TRUE)  %>% 
        head(10) %>% 
        ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
        scale_fill_manual(values = timepal) +
        geom_bar(stat = "identity", position = "dodge2") +
        labs (x =(input$name2), y = NULL) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_y_continuous(position = "right") 
      
      p1 + p2
        
    }
  )
  
})