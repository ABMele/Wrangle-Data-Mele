library(tidyverse)

x <- c("\"", "\\")
x

writeLines(x)

x <- c("apple", "banana", "pear")

str_view(x, "an")

str_view(x, ".a.")

str_view(x, "pp")

str_view(x, "a.")

slash <- "\\\\"

writeLines(slash)

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

x <- c("apple", "banana", "pear")
str_view(x, "^a")
#> [1] │ <a>pple
str_view(x, "a$")
#> [2] │ banan<a>

dol <- "\\$^$"

writeLines(dol)

corpus <- stringr::words

str_view(corpus, "^y")

str_view(corpus, "x$")

str_view(corpus, "^...$")

str_view(corpus, "^.......", match = TRUE)

str_view(corpus, "^a")

str_view(corpus, "^[^aeiou]+$")

str_view(corpus, "[^e]ed$")

str_view(corpus, "ing$|ise$")

writeLines("\\{.+\\}")

str_view(corpus, "\\\\{4}")

writeLines("\\\\{4}")

str_view(corpus, "^[^aeiou]{3}")

str_view(corpus, "[aeiou]{3,}")

install.packages("pdftools")

require(httr)

headers = c(
  `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
)

res <- httr::GET(url = 'https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf', httr::add_headers(.headers=headers))
library("pdftools")
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file, headers = headers)
getwd()
txt <- pdf_text("pnas_file.pdf")

file.remove(temp_file)

?download.file

res

raw_data_research <- txt[2]

raw_data_research %>% head(2)

tab <- str_split(raw_data_research, "\n")

tab <- tab[[1]]

tab

the_names_1 <- tab[3]
the_names_4 <- tab[5]

the_names_1 <- str_trim(the_names_1)

the_names_4 <- str_trim(the_names_4)

the_names_1

the_names_4

the_names_1 <- str_split(the_names_1, ",")

the_names_1 <- the_names_1[[1]]

the_names_4 <- str_split(the_names_4, "\\s+")
the_names_4 <- the_names_4[[1]]

the_names_4

the_names_1 <- str_replace(the_names_1, "\\sn\\s+", "")

the_names_1 <- the_names_1[-4]

the_names_1 <- str_replace(the_names_1, "\\s", "_")

the_new_names <- the_names_4[-1]

the_new_names <- str_c(the_names_1, the_new_names, sep = "_")

column_names <- c(the_names_4[1], the_new_names)

column_names

the_new_names

?paste0

?str_c

new_1 <- c(rep(the_names_1[1], 3), rep(the_names_1[2], 3), rep(the_names_1[3], 3))

the_new_names <- str_c(new_1, the_new_names, sep = "_")

new_2 <- c(rep(the_names_1, 3))

tab <- tab[-(1:5)]

tab <- tab[-1]

tab <- tab[-(12:33)]

tab <- tab[-11]

wrng <- str_split(tab, "\\s+", simplify = TRUE)

wrng

wrng <- str_trim(tab, side = "left")

wrng1

teststring <- wrng1[1]

test1 <- str_replace(str_sub(teststring, 1, -30), "\\s+", "_")

test1

wrng1 <- str_replace(wrng1[c(2,3,6,8,9,10)], "\\s+", "_")

wrng1

new_strng <- c(wrng1, wrng[c(1,4,5,7)])

x <- str_split(new_strng, "\\s+", simplify = TRUE)

first_clean <- x %>%
  as.data.frame() %>%
  setNames(column_names) %>%
  mutate_all(parse_guess)

str(first_clean)

nd_clean <- first_clean %>%
  str_replace(Success_rates_Men, "[ab]", "") %>%
  mutate(Success_rates_Men = as.numeric(Success_rates_Men))

nd_clean <- str_replace(first_clean$Success_rates_Men, "[ab]", "")
test1 <- first_clean %>%
  mutate(Success_rates_Women = as.numeric((str_replace(Success_rates_Women, "[ab]", ""))))

str(test1)

first_clean <- first_clean %>%
  mutate(Success_rates_Men = as.numeric((str_replace(Success_rates_Men, "[ab]", ""))),
         Success_rates_Women = as.numeric((str_replace(Success_rates_Women, "[ab]", ""))))

str(first_clean)

head(first_clean)

save(first_clean, file = "grant_applications.RData")

# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

?unnest

str_detect(19.5, "^1\\d*$")

library(rvest)

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

polls

col_names <- c("dates", "remain", "leave", "undecided", 
               "lead", "samplesize", "pollster", "poll_type", "notes")

new <- polls %>% 
  setNames(col_names) %>%
  filter(str_detect(remain, "%"))
head(new)

?parse_number

new <- new %>%
  mutate(undecided = str_replace(undecided, "N\\/A", "0"))

head(new)

now()

OlsonNames()

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)