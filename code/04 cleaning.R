
# Setup ###############################
library(tidyverse)
library(readxl)
library(openxlsx)

# Load data ###############
df_csv <- read.csv("output/combined_data.csv")  # must save data as csv in 02 fieldData 
na_percentage <- colMeans(is.na(df_csv)) * 100 # reading the csv from xlsx will change most values to logical 

# Selecting out 100% NAs
df_w <- df_csv %>%
  dplyr::select(-all_of(na_percentage[na_percentage == 100])) %>%
  dplyr::select(-x20, -x22, -x23, -x24, -x25, -x26)

