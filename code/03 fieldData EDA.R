library(tidyverse)
library(readxl)
library(openxlsx)
library(dplyr)

df <- read_excel("output/uncleanedCombined/combined data.xlsx")
# Load data as CSV ###################  is causing var to change to logical and others DONT? 
write.csv(df, file = "output/uncleanedCombined/combined data.csv", row.names = FALSE)
df_csv <- read.csv("output/uncleanedCombined/combined data.csv")  


# Finding columns with 100% NAs
na100 <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N=nrow(df_csv)) %>%
  mutate(prop = round(value/N*100,1)) %>%
  filter(prop == 100) %>%
  pull(name)


# Selecting out 100% NAs
df_w <- df %>%
  dplyr::select(-all_of(na100))  # consider removing x22, x23 also 


View(df_w)


df_w %>%
  dplyr::select(contains("star"), yr) #%>%
  drop_na() %>%
  summary
  View

  
df_csv %>%
df %>%
  filter(yr == 87) %>%
  View
  dplyr::select(contains("star"), yr) #%>%

test <- read_excel("data/fiber/1987 fibers.xlsx")
str(test)

str(df_csv)
str(df_w)


str(test)
