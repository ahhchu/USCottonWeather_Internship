
# Setup ###############################
library(tidyverse)
library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(readr)
# Load data ###############
df_csv <- read.csv("output/uncleanedCombined/combined data.csv")  # must save data as csv in 02 fieldData 
na_percentage <- colMeans(is.na(df_csv)) * 100 # reading the csv from xlsx will change most values to logical 

df <- read.xlsx("output/uncleanedCombined/combined data.xlsx")

# Selecting out ~100% NAs and removing those columns 
df_filtered <- df %>%
  dplyr::select_if(~!(all(is.na(.)))) 

#  rename mic to mic_star for years 1989->1992. 
years1 <- 89:92
df_filtered %>%
  mutate(mic_star = ifelse(yr %in% years1, mic, mic_star),
         mic = ifelse(yr %in% years1, NA, mic))

# for mic 
df_mic <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('mic_star', 'mic_spin', 'mic_st', 'mic_str', 'mic_mot', 'mic', 'mic_hvi'),
    names_to = "names",
    values_to = "value"
  ) %>% separate(names, sep = "_", into = c("mic", "lab")) %>%
  group_by(row_index, loc, yr, var, sample, breeder, regcode, reg, loccode, vcode, rep) %>%
  summarize(mean_mic = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index)
  
# for str
df_str <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('str_spin', 'str_mot', 'str_star', 'str'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("str", "lab")) %>%
  group_by(row_index, loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_str = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# for uhm
df_uhm <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('uhm_mot', 'uhm_spin', 'uhm'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("str", "lab")) %>%
  group_by(row_index,loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_uhm = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# for UI
df_ui <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('ui_spin', 'ui_mot', 'ui_star', 'ui'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("ui", "lab")) %>%
  group_by(row_index,loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_ui = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# for rd
df_rd <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('rd', 'rd_spin', 'rd_mot', 'rd_star'),
    names_to = "names",
    values_to = "value",
  ) %>% 
  separate(names, sep = "_", into = c("rd", "lab")) %>%
  group_by(row_index,loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_rd = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# for b
df_b <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('b', 'b_spin', 'b_mot', 'b_star'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("b", "lab")) %>%
  group_by(row_index,loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_b = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# x25sl 
df_x25sl <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('x25sl_star', 'x25sl'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("x25sl", "lab")) %>%
  group_by(row_index,loc, yr, var, sample, breeder, regcode, 
           reg, loccode, vcode, rep) %>%
  summarize(mean_x25sl = mean(value, na.rm = TRUE)) %>%
  arrange(row_index) %>%
  ungroup() %>%
  select(-row_index, -yr, -loc, -var, -sample, -breeder, -regcode, -reg, -loccode, -vcode, -rep)

# Merge #############

mean_df<- bind_cols(df_mic, df_rd, df_str, df_uhm, df_ui, df_b, df_x25sl)

# write to files
write_xlsx(mean_df, "output/mean/mean data.xlsx")
write_csv(mean_df, "output/mean/mean data.csv")

# write df_filtered to file (comparison purposes)
write_xlsx(df_filtered, "output/cleaned/filtered.xlsx")
write_csv(df_filtered, "output/cleaned/filtered.csv")





# TESTERS #####################
# compare 1980 - 1999
#mic 
mic_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  mic_count <- sum(df_mic$yr == year, na.rm = TRUE)
  mic_filtered_count <- c(mic_filtered_count, filtered_count, mic_count)
}
print(mic_filtered_count)
# str
str_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  str_count <- sum(df_str$yr == year, na.rm = TRUE)
  str_filtered_count <- c(str_filtered_count, filtered_count, str_count)
}
print(str_filtered_count)

#uhm
uhm_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  uhm_count <- sum(df_uhm$yr == year, na.rm = TRUE)
  uhm_filtered_count <- c(uhm_filtered_count, filtered_count, uhm_count)
}
print(uhm_filtered_count)

# ui
ui_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  ui_count <- sum(df_ui$yr == year, na.rm = TRUE)
  ui_filtered_count <- c(ui_filtered_count, filtered_count, ui_count)
}
print(ui_filtered_count)

# rd
rd_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  rd_count <- sum(df_rd$yr == year, na.rm = TRUE)
  rd_filtered_count <- c(rd_filtered_count, filtered_count, rd_count)
}
print(rd_filtered_count)

# b 
b_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  b_count <- sum(df_b$yr == year, na.rm = TRUE)
  b_filtered_count <- c(b_filtered_count, filtered_count, b_count)
}
print(b_filtered_count)


is_equal <- identical(mic_filtered_count, ui_filtered_count)
print(is_equal)


# 2000 - 2020
#mic 
mic_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  mic_count <- sum(df_mic$yr == year, na.rm = TRUE)
  mic_filtered_count <- c(mic_filtered_count, filtered_count, mic_count)
}
print(mic_filtered_count)
# str
str_filtered_count <- c()
for (year in 80:99) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  str_count <- sum(df_str$yr == year, na.rm = TRUE)
  str_filtered_count <- c(str_filtered_count, filtered_count, str_count)
}
print(str_filtered_count)

#uhm
uhm_filtered_count <- c()
for (year in 2000:2020) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  uhm_count <- sum(df_uhm$yr == year, na.rm = TRUE)
  uhm_filtered_count <- c(uhm_filtered_count, filtered_count, uhm_count)
}
print(uhm_filtered_count)

# ui
ui_filtered_count <- c()
for (year in 2000:2020) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  ui_count <- sum(df_ui$yr == year, na.rm = TRUE)
  ui_filtered_count <- c(ui_filtered_count, filtered_count, ui_count)
}
print(ui_filtered_count)

# rd
rd_filtered_count <- c()
for (year in 2000:2020) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  rd_count <- sum(df_rd$yr == year, na.rm = TRUE)
  rd_filtered_count <- c(rd_filtered_count, filtered_count, rd_count)
}
print(rd_filtered_count)

# b 
b_filtered_count <- c()
for (year in 2000:2020) {
  filtered_count <- sum(df_filtered$yr == year, na.rm = TRUE)
  b_count <- sum(df_b$yr == year, na.rm = TRUE)
  b_filtered_count <- c(b_filtered_count, filtered_count, b_count)
}
print(b_filtered_count)

is_equal <- identical(ui_filtered_count, ui_filtered_count)
print(is_equal)
