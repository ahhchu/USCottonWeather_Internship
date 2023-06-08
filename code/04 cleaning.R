
# Setup ###############################
library(tidyverse)
library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)

# Load data ###############
df_csv <- read.csv("output/combined data.csv")  # must save data as csv in 02 fieldData 
na_percentage <- colMeans(is.na(df_csv)) * 100 # reading the csv from xlsx will change most values to logical 

# Selecting out ~100% NAs and removing those columns 
df_filtered <- df_csv %>%
  dplyr::select_if(~!(all(is.na(.)))) %>%
  dplyr::select(-x20, -x22, -x23)

#  rename mic to mic_star for years 1989->1992. 
years1 <- 89:92
df_filtered %>%
  mutate(mic_star = ifelse(yr %in% years1, mic, mic_star),
         mic = ifelse(yr %in% years1, NA, mic))

# for mic 
df_mic <- df_filtered %>%
  mutate(row_index = row_number()) %>%
  pivot_longer(
    cols = c('mic_star', 'mic_spin', 'mic_st', 'mic_str', 'mic_mot', 'mic'),
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


# Merge #############

mean_df<- bind_cols(df_mic, df_rd, df_str, df_uhm, df_ui)

# using reduce 
df_list <- list(df_uhm, df_mic, df_str, df_ui, df_rd, df_b)
mean_df <- Reduce(function(x, y) merge(x, y, by = c("loc", "yr", "var", "sample", "breeder",  "reg", "loccode", "vcode", "rep", "regcode"), all = TRUE), df_list) %>%
  arrange(yr)

# using tidyverse 
df_list <- list(df_uhm, df_mic, df_str, df_ui, df_rd, df_b)
df_list %>% reduce(full_join, by='yr', 'loc')

# Convert data frames to data.tables
 dt_uhm <- as.data.table(df_uhm)
 dt_mic <- as.data.table(df_mic)
 dt_str <- as.data.table(df_str)
 dt_ui <- as.data.table(df_ui)
 dt_rd <- as.data.table(df_rd)
 dt_b <- as.data.table(df_b)
 mean_dt <- merge(dt_uhm, dt_mic, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
     merge(dt_str, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
     merge(dt_ui, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
     merge(dt_rd, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
     merge(dt_b, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
     .[order(yr)]  # Sort by "yr" column

 # merge as DF (original)
 mean_df <- merge(df_uhm, df_mic, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
   merge(df_str, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
   merge(df_ui, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
   merge(df_rd, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
   merge(df_b, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
   arrange(yr)

 
# write to files
write_xlsx(mean_df, "output/mean data.xlsx")
write_csv(mean_df, "output/mean data.csv")

# write df_filtered to file (comparison purposes)
write_xlsx(df_filtered, "output/filtered.xlsx")





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
