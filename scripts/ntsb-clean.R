library(tidyverse)
library(rvest)
source('https://raw.githubusercontent.com/declanrjb/declanrjb-r/refs/heads/main/functions.R')

drop_na_cols <- function(df) {
  return(df %>% select_if(function(vec) {length(which(!is.na(vec))) > 0}))
}

df <- list.files('data/rail/ntsb-scrape', full.names=TRUE) %>%
  lapply(read_csv) %>%
  do.call(rbind, .)

df <- df %>%
  select(!...1) 

colnames(df) <- colnames(df) %>%
  gsub("_x0020_", "", .) %>%
  gsub("NTSB", "NTSB_", .) %>%
  gsub("NSTB", "NTSB_", .)

df <- df %>%
  drop_na_cols()

df$WhatHappened <- df$WhatHappened %>% lapply(function(entry) {
  if (grepl('<', entry)) {
    entry %>% 
      read_html() %>% 
      html_text() %>%
      unlist() %>%
      return()
  } else {
    return(entry)
  }
}) %>%
  unlist()

df$Month <- df$AccidentDate %>% 
  month()

df$Day_Of_Week <- df$AccidentDate %>% 
  strftime("%a")

df$Time_Of_Day <- df$WhatHappened %>%
  str_extract("[0-9]{1,2}:[0-9]{2} [ap]\\.m\\.") %>%
  gsub('\\.', '', .) %>% 
  str_to_upper() %>% 
  parse_date_time(orders="%-I:%M %p")

write.csv(df, "data/ntsb-full_clean.csv", row.names=FALSE)

hours_table <- df$Time_Of_Day %>%
  hour() %>%
  value_counts() %>%
  arrange(Val)

hours_table <- hours_table %>%
  filter(!is.na(Val)) %>%
  mutate(Expected = 1/24)

# a spike occurs around 11am, just before lunch
ggplot(hours_table, aes(x=Val, y=Norm)) +
  geom_col() +
  theme_bw()

# conclude with significance that accidents are not evenly distributed by time of day (p = .0472)
chisq.test(hours_table$N, p = hours_table$Expected)

# slightly more at the hour but doesn't look significant
df %>%
  filter(!is.na(Time_Of_Day)) %>%
  pull(Time_Of_Day) %>%
  minute() %>%
  lapply(function(x) {floor(x / 10)}) %>%
  unlist() %>%
  value_counts() %>%
  arrange(Val)

# slightly more in january, february, and july
df$Month %>%
  value_counts() %>%
  arrange(Val)

# accidents are most likely on wednesdays
day_counts <- df$Day_Of_Week %>%
  value_counts() %>%
  mutate(Expected = 1/7)

# we can conclude with significance accidents are not evenly distributed throughout the week (p = .04447)
chisq.test(day_counts$N, p = day_counts$Expected)