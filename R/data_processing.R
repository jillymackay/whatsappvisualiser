# data processing


library(tidyverse)
library(readr)
library(tidytext)
library(zoo)


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



dat %>% 
  ggplot (aes (x = date)) +
  geom_bar (stat = "count")


dat %>% 
  separate (time, into = c("hours", "minutes"), sep =":") %>% 
  ggplot (aes (x = hours)) +
  geom_bar (stat = "count")
