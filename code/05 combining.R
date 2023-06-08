

# Setup ###############
library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(writexl)


# Loading Data ############ 
mean_df <- read_csv("output/mean/mean data.csv")
filtered_df <- read_csv("output/cleaned/filtered.csv")


filtered_df <-filtered_df %>%
  select(-ui_spin, -ui_star, -ui_mot, -ui, -rd_spin, -rd_star, -rd_mot, -rd,
         -uhm_spin, -uhm_mot, -uhm, -b_spin, -b_star, -b_mot, -b,
         -mic_spin, -mic_star, -mic_mot, -mic, -str_spin, -str_star, -str_mot, -str, -mic_str, -mic_st, -mic_hvi)

mean_df <- mean_df %>%
  select(mean_mic, mean_rd, mean_str, mean_uhm, mean_ui)
  


combined_df <- bind_cols(filtered_df, mean_df)

na80 <- combined_df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N = nrow(combined_df)) %>%
  mutate(prop = round(value / N * 100, 1)) %>%
  filter(prop > 95) %>%
  pull(name)

  
# cleaning ################################


# find what year code has values
code_values <- combined_df$code[!is.na(combined_df$code)]
code_yr <-  unique(combined_df$yr[!is.na(combined_df$code)])

# find what year scn_cnt has values
scn_cnt_values <- combined_df$scn_cnt[!is.na(combined_df$scn_cnt)]
scn_cnt_yr <- unique(combined_df$yr[!is.na(combined_df$scn_cnt)])

# find what year scn_cnt has values
scn_ct_values <- combined_df$scn_ct[!is.na(combined_df$scn_ct)]
scn_ct_yr <-  unique(combined_df$yr[!is.na(combined_df$scn_ct)])

# find what year sf_n has values
sf_n_values <- combined_df$sf_n[!is.na(combined_df$sf_n)]
sf_n_yr <-  unique(combined_df$yr[!is.na(combined_df$sf_n)])

# maturity
maturity_values <- combined_df$maturity[!is.na(combined_df$maturity)]
maturity_yr <-  unique(combined_df$yr[!is.na(combined_df$maturity)])

# mat_ratio is diff then maturity and mat 
mat_ratio__values <- combined_df$mat_ratio[!is.na(combined_df$mat_ratio)]
mat_ratio_yr <-  unique(combined_df$yr[!is.na(combined_df$mat_ratio)])

mat_values <- combined_df$mat[!is.na(combined_df$mat)]
mat_yr <-  unique(combined_df$yr[!is.na(combined_df$mat)])

# sfc_percent
sfc_percent_values <- combined_df$mat[!is.na(combined_df$sfc_percent)]
sfc_percent_yr <- unique(combined_df$yr[!is.na(combined_df$sfc_percent)])

sfc_values <- combined_df$sfc[!is.na(combined_df$sfc)]
sfc_yr <-  unique(combined_df$yr[!is.na(combined_df$sfc)])
                                      


# move code column into regcode 
combined_df <- combined_df %>%
  mutate(regcode = ifelse(yr %in% code_yr, code, regcode),
         code = ifelse(yr %in% code_yr, NA, code)) %>% 
  mutate(sfc_n = ifelse(yr %in% sf_n_yr, sf_n, sfc_n),
         sf_n = ifelse(yr %in% sf_n_yr, NA, sf_n)) %>%
  mutate(sfc_n = ifelse(yr %in% sf_n_yr, sf_n, sfc_n),
         sf_n = ifelse(yr %in% maturity_yr, NA, sf_n)) %>%
  mutate(mat = ifelse(yr %in% maturity_yr, maturity, mat),
         maturity = ifelse(yr %in% sf_n_yr, NA, maturity)) %>%
  mutate(sfc = ifelse(yr %in% 2019, sfc_percent, sfc),
         sfc_percent = ifelse(yr %in% 2019, NA, sfc_percent)) %>%
  select(-code, -sf_n, -maturity, -sfc_percent)

View(combined_df)

write.xlsx(combined_df, "output/cleaned/tester.xlsx")


# which variables are still over 80% NA
na80 <- combined_df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N = nrow(combined_df)) %>%
  mutate(prop = round(value / N * 100, 1)) %>%
  filter(prop > 80) %>%
  pull(name)


  
  
  
  
  
  
  
