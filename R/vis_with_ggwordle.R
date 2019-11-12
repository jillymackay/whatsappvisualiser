# ggwordle vis

library(tidyverse)
library(ggwordcloud)
library(wesanderson)







# why no work?


dat %>% 
  filter(str_detect(word, '[^0-9]')) %>% 
  count (word, shortsend, sort = TRUE) %>%
  top_n(100)  %>% 
  ggplot(aes(label = word, size = n, color = shortsend)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_color_manual(values = wes_palette("Darjeeling2"))

# this works

tibble(word = c("A", "B", "C", "D"),
       size = c(4,3,2,1)) %>% 
  ggplot(aes(label = word, size = size)) +
  geom_text_wordcloud_area() +
  theme_minimal()
