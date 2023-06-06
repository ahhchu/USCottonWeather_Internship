library(tidyverse)
library(readxl)
library(openxlsx)

df <- read.xlsx("output/combined_data.xlsx")

# Loading data
df <- read_excel("output/combined_data.xlsx")

# Load data as CSV ################### 
write.csv(df, file = "output/combined_data.csv", row.names = FALSE)
df_csv <- read.csv("output/combined_data.csv")  

# Finding columns with 100% NAs
na100 <- df_csv %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N=nrow(df)) %>%
  mutate(prop = round(value/N*100,1)) %>%
  filter(prop == 100) %>%
  pull(name)

# Selecting out 100% NAs
df_w <- df_csv %>%
  dplyr::select(-all_of(na100)) %>% # consider removing x22, x23 also 
  dplyr::select(-x20, -x22, -x23)
View(df_w)


df_w %>%
  dplyr::select(contains("star"), yr) #%>%
  drop_na() %>%
  summary
  View

  
df_csv %>%
  filter(yr == 87) %>%
  View
  dplyr::select(contains("star"), yr) #%>%

test <- read_excel("data/fiber/1987 fibers.xlsx")
str(test)

str(df_csv)
str(df_w)



