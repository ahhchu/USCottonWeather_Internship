

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
  filter(prop > 85) %>%
  pull(name)

  
# cleaning ################################


# find what year code has values
code_values <- combined_df$code[!is.na(combined_df$code)]
code_yr <-  unique(combined_df$yr[!is.na(combined_df$code)])

# find what year sf_n has values
sf_n_values <- combined_df$sf_n[!is.na(combined_df$sf_n)]
sf_n_yr <-  unique(combined_df$yr[!is.na(combined_df$sf_n)])

# maturity
maturity_values <- combined_df$maturity[!is.na(combined_df$maturity)]
maturity_yr <-  unique(combined_df$yr[!is.na(combined_df$maturity)])

# mat
mat_values <- combined_df$mat[!is.na(combined_df$mat)]
mat_yr <-  unique(combined_df$yr[!is.na(combined_df$mat)])


# mat_ratio is diff then maturity and mat 
mat_ratio__values <- combined_df$mat_ratio[!is.na(combined_df$mat_ratio)]
mat_ratio_yr <-  unique(combined_df$yr[!is.na(combined_df$mat_ratio)])

# mat_rat 
mat_rat_values <- combined_df$mat_rat[!is.na(combined_df$mat_rat)]
mat_rat_yr <-  unique(combined_df$yr[!is.na(combined_df$mat_rat)])

# sfc_percent
sfc_percent_values <- combined_df$mat[!is.na(combined_df$sfc_percent)]
sfc_percent_yr <- unique(combined_df$yr[!is.na(combined_df$sfc_percent)])

sfc_values <- combined_df$sfc[!is.na(combined_df$sfc)]
sfc_yr <-  unique(combined_df$yr[!is.na(combined_df$sfc)])
                                      
# len_w
len_w_values <- combined_df$len_w[!is.na(combined_df$len_w)]
len_w_yr <-  unique(combined_df$yr[!is.na(combined_df$len_w)])

# length
length_values <- combined_df$length[!is.na(combined_df$length)]
length_yr <-  unique(combined_df$yr[!is.na(combined_df$length)])

# x50sl
x50sl_values <- combined_df$x50sl[!is.na(combined_df$x50sl)]
x50sl_yr <-  unique(combined_df$yr[!is.na(combined_df$x50sl)])

# x50_sl 
x50_sl_values <- combined_df$x50_sl[!is.na(combined_df$x50_sl)]
x50_sl_yr <-  unique(combined_df$yr[!is.na(combined_df$x50_sl)])

# nep_cnt 
nep_cnt_values <- combined_df$nep_cnt[!is.na(combined_df$nep_cnt)]
nep_cnt_yr <-  unique(combined_df$yr[!is.na(combined_df$nep_cnt)])


# nepct
nepct_values <- combined_df$nepct[!is.na(combined_df$nepct)]
nepct_yr <-  unique(combined_df$yr[!is.na(combined_df$nepct)])

# nep 
nep_values <- combined_df$nep[!is.na(combined_df$nep)]
nep_yr <-  unique(combined_df$yr[!is.na(combined_df$nep)])

# x25sl
x25sl_values <- combined_df$x25sl[!is.na(combined_df$x25sl)]
x25sl_yr <-  unique(combined_df$yr[!is.na(combined_df$x25sl)])

# x25_sl 
x25_sl_values <- combined_df$x25_sl[!is.na(combined_df$x25_sl)]
x25_sl_yr <-  unique(combined_df$yr[!is.na(combined_df$x25_sl)])

# x25sl_star
x25sl_star_values <- combined_df$x25sl_star[!is.na(combined_df$x25sl_star)]
x25sl_star_yr <-  unique(combined_df$yr[!is.na(combined_df$x25sl_star)])

# elg_star 
elg_star_values <- combined_df$elg_star[!is.na(combined_df$elg_star)]
elg_star_yr <-  unique(combined_df$yr[!is.na(combined_df$elg_star)])

# elg_hvi
elg_hvi_values <- combined_df$elg_hvi[!is.na(combined_df$elg_hvi)]
elg_hvi_yr <-  unique(combined_df$yr[!is.na(combined_df$elg_hvi)])

# elg 
elg_values <- combined_df$elg[!is.na(combined_df$elg)]
elg_yr <-  unique(combined_df$yr[!is.na(combined_df$elg)])

# elo 
elo_values <- combined_df$elo[!is.na(combined_df$elo)]
elo_yr <-  unique(combined_df$yr[!is.na(combined_df$elo)])

# elong
elong_values <- combined_df$elong[!is.na(combined_df$elong)]
elong_yr <-  unique(combined_df$yr[!is.na(combined_df$elong)])

# len_n
len_n_values <- combined_df$len_n[!is.na(combined_df$len_n)]
len_n_yr <-  unique(combined_df$yr[!is.na(combined_df$len_n)])

#l_n
l_n_values <- combined_df$l_n[!is.na(combined_df$l_n)]
l_n_yr <-  unique(combined_df$yr[!is.na(combined_df$l_n)])

# move code column into regcode 
combined_df <- combined_df %>%
  mutate(regcode = ifelse(yr %in% code_yr, code, regcode), # code -> regcode 
         code = ifelse(yr %in% code_yr, NA, code)) %>% 
  mutate(sfc_n = ifelse(yr %in% sf_n_yr, sf_n, sfc_n), # sf_n -> sfc_n
         sf_n = ifelse(yr %in% sf_n_yr, NA, sf_n)) %>%
  mutate(mat = ifelse(yr %in% maturity_yr, maturity, mat), # maturity -> mat 
         maturity = ifelse(yr %in% sf_n_yr, NA, maturity)) %>%
  mutate(sfc = ifelse(yr %in% 2019, sfc_percent, sfc), # sfc_percent -> sfc
         sfc_percent = ifelse(yr %in% 2019, NA, sfc_percent)) %>%
  mutate(length = ifelse(yr %in% len_w_yr, len_w, length), # len_w -> length 
         len_w = ifelse(yr %in% len_w_yr, NA, len_w)) %>%
  mutate(mat_ratio = ifelse(yr %in% mat_rat_yr, mat_rat, mat_ratio), # mat_rat -> mat_ratio 
         mat_rat = ifelse(yr %in% mat_rat_yr, NA, mat_rat)) %>%
  mutate(x50sl = ifelse(yr %in% x50_sl_yr, x50_sl, x50sl), # x50_sl -> x50sl 
         x50_sl = ifelse(yr %in% x50_sl_yr, NA, x50_sl)) %>%
  mutate(nep = ifelse(yr %in% nep_cnt_yr, nep_cnt, nep), # nep_cnt -> nep 
         nep_cnt = ifelse(yr %in% nep_cnt_yr, NA, nep_cnt)) %>%
  mutate(nep = ifelse(yr %in% nepct_yr, nepct, nep), # nepct -> nep 
         nepct = ifelse(yr %in% nepct_yr, NA, nepct)) %>%
  mutate(elg = ifelse(yr %in% elg_star_yr, elg_star, elg), # elg_star -> elg
         elg_star = ifelse(yr %in% elg_star_yr, NA, elg_star)) %>%
  mutate(elg = ifelse(yr %in% elg_hvi_yr, elg_hvi, elg), # elg_hvi -> elg
         elg_hvi = ifelse(yr %in% elg_hvi_yr, NA, elg_hvi)) %>%
  mutate(elg = ifelse(yr %in% elong_yr, elong, elg), # elong -> elg
         elong = ifelse(yr %in% elong_yr, NA, elong)) %>%
  mutate(elg = ifelse(yr %in% elo_yr, elo, elg), # elg_hvi -> elg
         elo = ifelse(yr %in% elo_yr, NA, elo)) %>%
  mutate(x25sl = ifelse(yr %in% x25_sl_yr, x25_sl, x25sl), # x25_sl -> x25sl
         x25_sl = ifelse(yr %in% x25_sl_yr, NA, x25_sl)) %>%
  mutate(l_n = ifelse(yr %in% len_n_yr, len_n, l_n), # len_n -> l_n
         len_n = ifelse(yr %in% len_n_yr, NA, len_n)) %>%
  select(-code, -sf_n, -maturity, -sfc_percent, -len_w, -mat_rat, -x50_sl, -nep_cnt, -nepct, -elg_hvi, -elong, -elo, -x25sl_star, -x20, -x22, -x23, -elg_star, -x25_sl, -len_n)

View(combined_df)

write.xlsx(combined_df, "output/cleaned/cleaned.xlsx")
write.csv(combined_df, "output/cleaned/cleaned.cvs")


# which variables are still over 80% NA
na80 <- combined_df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(N = nrow(combined_df)) %>%
  mutate(prop = round(value / N * 100, 1)) %>%
  filter(prop > 80) %>%
  pull(name)


  
  
  
  
  
  
  
