library(tidyverse)
library(readxl)

# Loading data
df <- read_excel("output/combined_data.xlsx")

# Finding columns with 100% NAs
na100 <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N=nrow(df)) %>%
  mutate(prop = round(value/N*100,1)) %>%
  filter(prop == 100) %>%
  pull(name)

# Selecting out 100% NAs
df_w <- df %>%
  dplyr::select(-all_of(na100))

View(df_w)


df_w %>%
  dplyr::select(contains("star"), yr) #%>%
  drop_na() %>%
  summary
  View

  
df %>%
  filter(yr == 87) %>%
  View
  dplyr::select(contains("star"), yr) #%>%

test <- read_excel("data/fiber/1987 fibers.xlsx")
test

str(test)
