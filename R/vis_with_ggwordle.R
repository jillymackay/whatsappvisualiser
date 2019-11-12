# ggwordle vis

library(tidyverse)
library(ggwordcloud)
library(wesanderson)







# why no work?


dat %>% 
  filter(str_detect(word, '[^0-9]')) %>% 
  count (word, shortsend, sort = TRUE) %>%
  top_n(5)  %>% 
  ggplot(aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()
  


tibble(word = c("A", "B", "C", "D"),
       size = c(4,3,2,1)) %>% 
  ggplot(aes(x = word, y = size)) +
  geom_bar(stat = "identity")
