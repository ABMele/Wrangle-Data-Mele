library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1, sample.kind = "Rounding")

url <- 'http://drive.google.com/uc?export=download&id=16wm-2NTKohhcA26w-kaWfhLIGwl_oX95'
trump_tweets <- jsonlite::fromJSON(txt = url, simplifyDataFrame = TRUE) %>%
  rename(source = device,
         id_str = id,
         created_at = date,
         retweet_count = retweets,
         favorite_count = favorites,
         is_retweet = isRetweet) %>%
  mutate(created_at = as.POSIXct(created_at, tz = "EST"),
         id_str = as.character(id_str),
         is_retweet = is_retweet == "t") %>%
  filter(!is_retweet & !str_detect(text, '^"') &
           created_at >= as.POSIXct("2009-05-04 13:54:25", tz = "EST") &
           created_at <= as.POSIXct("2018-01-01 08:37:52", tz = "EST")) %>%
  select(source, id_str, text, created_at, retweet_count, favorite_count, is_retweet) 

head(trump_tweets)

?trump_tweets

trump_tweets %>%
  count(source) %>%
  arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

?extract

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

install.packages("tidytext")

library(tidytext)

example <- data.frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

head(trump_tweets)

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

tweet_words %>%
  count(word, source)

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

get_sentiments("bing")

?sentiments

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

install.packages("nrc")

install.packages("sentiments")

install.packages("textdata")

library(textdata)

get_sentiments("nrc")

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

options(digits = 3)   

data("brexit_polls")

str(brexit_polls)

brexit_polls %>% 
  filter(str_detect(startdate, "....-04-.."))

?round_date

brexit_polls %>% 
  mutate(enddate = round_date(enddate, unit = "week")) %>%
  filter(enddate == "2016-06-12")

brexit_polls %>%
  mutate(weekday = weekdays(enddate)) %>%
  count(weekday) %>%
  arrange(desc(n))

data("movielens")

movietest <- movielens %>%
  mutate(timestamp = as_datetime(timestamp))

str(movietest)

movietest1 <- movietest %>%
  mutate(year = year(timestamp), hour = hour(timestamp)) %>%
  count(hour) %>%
  arrange(desc(n))

install.packages("gutenbergr")

library(gutenbergr)

gutenberg_metadata

ind <- str_detect(gutenberg_metadata$title, "Pride and Prejudice")

gutenberg_metadata$gutenberg_id[ind]

?gutenberg_works

gutenberg_works(title == "Pride and Prejudice")

pride_book <- gutenberg_download(1342, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")

prebook <- str_split(pride_book$text, "-")

word_test2 <- prebook %>%
  unnest_tokens([[1:14529]], word) %>%
  select(word)

as.data.frame(prebook[[1:14529]])

?gutenberg_download

head(pride_book)

prebook <- pride_book %>%
  mutate(text = str_replace_all(text, "-", " "))

patti <- "[^A-Za-z\\d\\W]"

words_test <-pride_book %>%
  unnest_tokens(word, text) %>%
  select(word) %>%
  count(word) %>%
  summarise(sum(n))

head(words_test)
view(words_test)
words_test
?unnest_tokens

str_view(words_test, "-")

sum(str_detect(words_test$word, "-"))

words_test2 <- prebook %>%
  unnest_tokens(word, text)%>%
  select(word)

view(prebook)

words_test <- words_test %>%
  filter(!word%in%stop_words$word)

words_test <- words_test %>%
  filter(!str_detect(word, "\\d+"))

words_test %>%
  count(word) %>%
  filter(n > 100) %>%
  arrange(desc(n))

afinn <- get_sentiments("afinn")

left_join()

senti_test <- words_test %>%
  left_join(afinn, by = "word")

senti_test %>%
  group_by(value) %>%
  summarise(no = n())

senti_test %>%
  filter(!is.na(value)) %>%
  filter(value > 0)

senti_test %>%
  filter(value == 4)
getwd()
senti_test %>%
  filter(value > 0)

3560/6353

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

txt <- pdf_text(fn)

txt

txt_2 <- txt[9]

x <- str_split(txt_2, "\n")

x

s <- x[[1]]

s_2 <- str_trim(s)

s_2

header_index <- str_which(s_2, "2015")

header <- s_2[header_index[1]]

header <- str_split(header, "\\s+", simplify = TRUE)

month <- header[1, 1]

header <- header[,-1]

tail_index <- 36

digit_count <- str_count(s_2, pattern = "\\d+")

sum(digit_count == 1)

s_2 <- s_2[-c(1,2,3,7,10,36:41)]

new_s <- str_replace_all(s_2, "[^\\d\\s]", "")

new_s <- str_split_fixed(new_s, "\\s+", n = 6)[,1:5]

new_s

tab <- new_s %>%
  as.data.frame() %>%
  setNames(c("day", header)) %>%
  mutate_all(parse_number) %>%
  mutate(month = month)

head(tab)

mean(tab$`2016`)

tab %>%
  filter(day%in%c(20:30)) %>%
  select(`2017`) %>%
  summarise(avg = mean(`2017`))

united_tab <- tab%>%
  gather(year, deaths, -c(day, month)) %>%
  mutate(deaths = as.numeric(deaths))

united_tab
?unite

?gather

united_tab %>%
  filter(!year == 2018) %>%
  ggplot(aes(day, deaths, colour = year)) + geom_line() + 
  geom_vline(xintercept = 20)

write_csv(united_tab, "pc_hurricane_data.csv")
