# data processing


library(tidyverse)
library(readr)
library(tidytext)
library(zoo)





dat <- read_lines("data/WhatsApp.txt") %>% 
  tibble() %>% 
  rename(text = ".")


dat1 <- dat %>% 
  mutate (text = parse_character(text)) %>% 
  mutate (speaker = case_when (str_detect(text, "Smith") ~ "Fraser",
                               str_detect(text, "Jilly MacKay") ~ "Jilly",
                               TRUE ~ "none"))
# this is far from perfect yet!


dat1 %>% 
  unnest_tokens(word, text)  %>% 
  count(word, sort = TRUE) %>% 
  anti_join(stop_words) %>% 
  head(10) %>% 
  ggplot(aes(word, n)) +
  geom_col()

dat2 <- dat1 %>% 
  mutate(is_time = str_detect(text, "\d\d\/\d\d\/\d\d\d\d\,\ \d\d\:\d\d"),
         time = ifelse(is_time, text, NA),
         time = na.locf(time, na.rm = FALSE))

  