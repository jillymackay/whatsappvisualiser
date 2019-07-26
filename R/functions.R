# Functions


library(tidyverse)
library(readr)
library(tidytext)
library(zoo)
library(textstem)
library(wesanderson)
library(wordcloud)
library(patchwork)

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
