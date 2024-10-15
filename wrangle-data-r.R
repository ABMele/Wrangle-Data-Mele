library(tidyverse)

murders <- read_csv("who_clean.csv")

murders <- mutate()

data("murders")

murders <- murders %>%
  mutate(rate = total/population * 100000)

save(murders, file = "rda/murders.rda")

load("rda/murders.rda")

