# multiple person chats


library(tidyverse)
library(readr)
library(tidytext)
library(zoo)
library(textstem)
library(wesanderson)


dat <- read_lines("C:/Users/jmackay4/Desktop/bbbchat.txt") %>% 
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


my_stops <- tibble(word = c("omit", "medium", "https", "i ' be", "fbclid"),
                   lexicon = c("my_stops", "my_stops", "my_stops", "mystops", "mystops"))
my_stops<-rbind(stop_words, my_stops)


words <- dat %>% 
  mutate (shortsend = case_when (str_detect(sender, "Alex") ~ "Alex",
                                 str_detect(sender, "Jilly") ~ "Jilly",
                                 str_detect(sender, "Carol") ~ "Carol",
                                 str_detect(sender, "Jo") ~ "Jo",
                                 str_detect(sender, "Kay") ~ "Kay",
                                 str_detect(sender, "Lucille") ~ "Lucille",
                                 str_detect(sender, "Sarah") ~ "Sarah",
                                 str_detect(sender, "Susan") ~ "Susan",
                                 str_detect(sender, "Suzanne") ~ "Suzanne")) %>% 
  unnest_tokens(word, msg_text) %>% 
  mutate (word = (lemmatize_strings(word))) %>% 
  anti_join(my_stops) %>% 
  filter(str_detect(word, '[^0-9]'))





vis <- dat %>% 
  mutate (shortsend = case_when (str_detect(sender, "Alex") ~ "Alex",
                                 str_detect(sender, "Jilly") ~ "Jilly",
                                 str_detect(sender, "Carol") ~ "Carol",
                                 str_detect(sender, "Jo") ~ "Jo",
                                 str_detect(sender, "Kay") ~ "Kay",
                                 str_detect(sender, "Lucille") ~ "Lucille",
                                 str_detect(sender, "Sarah") ~ "Sarah",
                                 str_detect(sender, "Susan") ~ "Susan",
                                 str_detect(sender, "Suzanne") ~ "Suzanne"))

timepal <- wes_palette("Darjeeling2", 24, type = "continuous")
bpal <- wes_palette("Darjeeling2", 10, type = "continuous")



vis %>% 
  ggplot (aes (x = date, fill = shortsend)) +
  geom_bar (stat = "count", position = "dodge2") +
  theme_classic() +
  scale_fill_manual(values = bpal ) +
  theme_classic() +
  theme(legend.position = "none") +
  labs (x = "Date", y= "n messages", title = "WhatsApp message history for 9 person chat") +
  facet_grid(rows = vars(shortsend))


p4 <- vis %>% 
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
  comparison.cloud(colors =  bpal, max.words = 150, 
                   rot.per = 0, use.r.layout = TRUE)



words %>% 
  ggplot(aes(label = word,
             colour = shortsend)) +
  geom_text_wordcloud_area() + 
  theme_minimal()






# developing a nice by person frequent word visualisation


whatplot_words <- function (data, sender1, sender2) {
  pal <- wes_palette("GrandBudapest2", 10, "continuous")
  

  sender1 <- enquo(sender1)
  sender2 <- enquo(sender2)
  
  p1 <- data %>% 
    filter(shortsend == !!sender1) %>% 
    count(word, shortsend, sort = TRUE)  %>% 
    head(10) %>% 
    ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
    geom_bar(stat = "identity", position = "dodge2") +
    scale_fill_manual(values = pal) +
    labs (x = !!sender1, y = NULL) +
    theme_classic() +
    theme(legend.position = "none") 
  
  
  p2 <-words %>% 
    filter(shortsend == !!sender2) %>% 
    count(word, shortsend, sort = TRUE)  %>% 
    head(10) %>% 
    ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
    scale_fill_manual(values = pal) +
    geom_bar(stat = "identity", position = "dodge2") +
    labs (x = !!sender2, y = NULL) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_y_continuous(position = "right") 
  
  p1 + p2
}

whatplot_words(words, "Suzanne", "Jill")


sooz <- words %>% 
  filter(shortsend == "Suzanne") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_fill_manual(values = timepal) +
  labs (x = "Suzanne", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") 


jill <-words %>% 
  filter(shortsend == "Jilly") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
  scale_fill_manual(values = timepal) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs (x = "Jilly", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right") 


carol <- words %>% 
  filter(shortsend == "Carol") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
  scale_fill_manual(values = timepal) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs (x = "Carol", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right") 

kay <- words %>% 
  filter(shortsend == "Kay") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
  scale_fill_manual(values = timepal) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs (x = "Kay", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right") 

sarah <- words %>% 
  filter(shortsend == "Sarah") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
  scale_fill_manual(values = timepal) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs (x = "Sarah", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right") 

alex <- words %>% 
  filter(shortsend == "Alex") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_fill_manual(values = timepal) +
  labs (x = "Alex", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") 


Jo<- words %>% 
  filter(shortsend == "Jo") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_fill_manual(values = timepal) +
  labs (x = "Jo", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") 

lucille <- words %>% 
  filter(shortsend == "Lucille") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, -n), y= n, fill = word)) +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_fill_manual(values = timepal) +
  labs (x = "Lucille", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") 

susan <- words %>% 
  filter(shortsend == "Susan") %>% 
  count(word, shortsend, sort = TRUE)  %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y= n, fill = word)) +
  scale_fill_manual(values = timepal) +
  geom_bar(stat = "identity", position = "dodge2") +
  labs (x = "Susan", y = NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(position = "right") 

alex + carol
Jo + kay
lucille + sarah
sooz + susan


(alex + carol) | (Jo + kay)
