library(wesanderson)
library(wordcloud)

timepal <- wes_palette("Moonrise3", 24, type = "continuous")

vis <- dat %>% 
  mutate (shortsend = case_when (str_detect(sender, "Fraser") ~ "Fraser",
                                 str_detect(sender, "Jilly") ~ "Jilly"))

vis %>% 
  ggplot (aes (x = date, fill = shortsend)) +
  geom_bar (stat = "count", position = "dodge2") +
  theme_classic() +
  scale_fill_manual(values = wes_palette("Moonrise3")) +
  theme_classic() +
  theme(legend.position = "none") +
  labs (x = "Date", y= "n messages", title = "WhatsApp message history for 2 person chat")


vis %>% 
  separate (time, into = c("hours", "minutes"), sep =":") %>% 
  ggplot (aes (x = hours, fill = hours)) +
  geom_bar (stat = "count") +
  scale_fill_manual(values = timepal) +
  theme_classic() +
  theme(legend.position = "none") +
  labs (x = "Hour", y = "n messages")
  

words %>%
  filter(str_detect(word, '[^0-9]')) %>% 
  count (word, shortsend, sort = TRUE) %>%
  reshape2::acast(word ~ shortsend, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#533366",    "#E9967A"), max.words = 150, 
                   rot.per = 0, use.r.layout = FALSE)


words %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = word, y= n, fill = shortsend)) +
  geom_bar(stat = "identity", position = "dodge2")

