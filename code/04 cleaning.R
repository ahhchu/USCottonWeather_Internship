
# Setup ###############################
library(tidyverse)
library(readxl)
library(openxlsx)
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
  pivot_longer(
    cols = c('mic_star', 'mic_spin', 'mic_st', 'mic_str', 'mic_mot', 'mic'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("mic", "lab")) %>%
  group_by(loc, yr, var, sample, breeder, reg) %>%
  summarize(mean_mic = mean(value, na.rm = TRUE)) %>%
  arrange(yr)

  
# for str
df_str <- df_filtered %>%
  pivot_longer(
    cols = c('str_spin', 'str_mot', 'str_star', 'str'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("str", "lab")) %>%
  group_by(loc, yr, var, sample, breeder) %>%
  summarize(mean_str = mean(value, na.rm = TRUE)) %>%
  arrange(yr)

# for uhm
df_uhm <- df_filtered %>%
  pivot_longer(
    cols = c('uhm_mot', 'uhm_spin', 'uhm'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("str", "lab")) %>%
  group_by(loc, yr, var, sample, breeder) %>%
  summarize(mean_uhm = mean(value, na.rm = TRUE)) %>%
  arrange(yr)

# for UI
df_ui <- df_filtered %>%
  pivot_longer(
    cols = c('ui', 'ui_spin', 'ui_mot', 'ui_star'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("ui", "lab")) %>%
  group_by(loc, yr, var, sample, breeder) %>%
  summarize(mean_ui = mean(value, na.rm = TRUE)) %>%
  arrange(yr)

# for rd
df_rd <- df_filtered %>%
  pivot_longer(
    cols = c('rd', 'rd_spin', 'rd_mot', 'rd_star'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("rd", "lab")) %>%
  group_by(loc, yr, var, sample, breeder) %>%
  summarize(mean_rd = mean(value, na.rm = TRUE)) %>%
  arrange(yr)

# for b
df_b <- df_filtered %>%
  pivot_longer(
    cols = c('b', 'b_spin', 'b_mot', 'b_star'),
    names_to = "names",
    values_to = "value",
  ) %>% separate(names, sep = "_", into = c("b", "lab")) %>%
  group_by(loc, yr, var, sample, breeder) %>%
  summarize(mean_rd = mean(value, na.rm = TRUE)) %>%
  arrange(yr)



# merging mic, uhm, str means into one df 
mean_df <- merge(df_uhm, df_mic, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
  merge(df_str, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
  merge(df_ui, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
  merge(df_rd, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
  merge(df_b, by = c("loc", "yr", "var", "sample", "breeder"), all = TRUE) %>%
  arrange(yr)

View(mean_df)

write_xlsx(mean_df, "output/mean data.xlsx")


