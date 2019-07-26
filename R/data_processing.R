# data processing


library(tidyverse)
library(readr)
library(tidytext)
library(zoo)
library(textstem)
library(wesanderson)


dat <- read_lines("data/WhatsApp.txt") %>% 
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
  mutate (date = lubridate::dmy(date)) 


my_stops <- tibble(word = c("omit", "medium", "https"),
                   lexicon = c("my_stops", "my_stops", "my_stops"))
my_stops<-rbind(stop_words, my_stops)


words <- dat %>% 
  mutate (shortsend = case_when (str_detect(sender, "Fraser") ~ "Fraser",
                                 str_detect(sender, "Jilly") ~ "Jilly")) %>% 
  unnest_tokens(word, msg_text) %>% 
  mutate (word = (lemmatize_strings(word))) %>% 
  anti_join(my_stops) %>% 
  filter(str_detect(word, '[^0-9]'))
 


words %>% 
  count(word, shortsend, sort = TRUE)
