

# Setup ################
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
library(readr)
library(dplyr)

# Load Data ##########
directory_path <- "data/fiber"
file_names <- list.files(directory_path, pattern = "\\d{4} fibers\\.xlsx$", full.names = TRUE)
file_names  # file names has all the xlsx stored in a list


# Wrangling ####################
for (file in file_names) {
  data <- read_excel(file)  
  data <- data %>% clean_names()
  csv_file <- sub(".xlsx", ".csv", file)  
  write.csv(data, csv_file, row.names = FALSE) # create csv 
}

csv_names <- list.files(directory_path, pattern = "\\d{4} fibers\\.csv$", full.names = TRUE)
combined_data <- data.frame()

# Function to rename columns based on conditions
rename_columns <- function(data) {
  if ("variety" %in% colnames(data)) {
    colnames(data)[colnames(data) == "variety"] <- "var"
  }
  if ("varcode" %in% colnames(data)) {
    colnames(data)[colnames(data) == "varcode"] <- "vcode"
  }
  if ("location" %in% colnames(data)) {
    colnames(data)[colnames(data) == "location"] <- "loc"
  }
  if ("lococde" %in% colnames(data)) {
    colnames(data)[colnames(data) == "lococde"] <- "loccode"
  }
  if ("lo_ccode" %in% colnames(data)) {
    colnames(data)[colnames(data) == "lo_ccode"] <- "loccode"
  }
  if ("samples" %in% colnames(data)) {
    colnames(data)[colnames(data) == "samples"] <- "sample"
  }
  if ("re_gcode" %in% colnames(data)) {
    colnames(data)[colnames(data) == "re_gcode"] <- "regcode"
  }
  if ("reg_code" %in% colnames(data)) {
    colnames(data)[colnames(data) == "reg_code"] <- "regcode"
  }
  if ("str_mpt" %in% colnames(data)) {
    colnames(data)[colnames(data) == "str_mpt"] <- "str_mot"
  }
  if ("sam" %in% colnames(data)) {
    colnames(data)[colnames(data) == "sam"] <- "sample"
  }
  if ("sample_number" %in% colnames(data)) {
    colnames(data)[colnames(data) == "sample_number"] <- "sample"
  }
  if ("hunterb" %in% colnames(data)) {
    colnames(data)[colnames(data) == "hunterb"] <- "b"
  }
  if ("plusb" %in% colnames(data)) {
    colnames(data)[colnames(data) == "plusb"] <- "b"
  } 
  if ("plus_b" %in% colnames(data)) {
    colnames(data)[colnames(data) == "plus_b"] <- "b"
  }
  if ("hunters" %in% colnames(data)) {
    colnames(data)[colnames(data) == "hunters"] <- "b"
  }
  if ("name" %in% colnames(data)) {
    colnames(data)[colnames(data) == "name"] <- "var"
  }
  return(data)
}

# Loop over the CSV files and create df
for (i in 1:41) {
  file <- csv_names[i]
  df_name <- paste0("data", i)  # Generate the data frame name
  
  if (file.exists(file)) {
    assign(df_name, read_csv(file))
    assign(df_name, rename_columns(get(df_name)))  # Apply column renaming
  } else {
    cat("File does not exist:", file, "\n")
  }
}

# add year values 
data21$yr <- ifelse(data21$yr == 0, 2000, data21$yr)
data22$yr <- ifelse(data22$yr == 1, 2001, data22$yr)
data23$yr <- ifelse(data23$yr == 2, 2002, data23$yr)
data24$yr <- ifelse(data24$yr == 3, 2003, data24$yr)
data25$yr <- ifelse(data25$yr == 4, 2004, data25$yr)
data26$yr <- ifelse(data26$yr == 5, 2005, data26$yr)
data27$yr <- ifelse(data27$yr == 6, 2006, data27$yr)
data28$yr <- ifelse(data28$yr == 7, 2007, data28$yr)
data29$yr <- ifelse(data29$yr == 8, 2008, data29$yr)
data30$yr <- ifelse(data30$yr == 9, 2009, data30$yr)
data31$yr <- ifelse(data31$yr == 10, 2010, data31$yr)
data32$yr <- ifelse(data32$yr == 11, 2011, data32$yr)
data33$yr <- ifelse(data33$yr == 12, 2012, data33$yr)
data34$yr <- ifelse(data34$yr == 13, 2013, data34$yr)
data35$yr <- ifelse(data35$yr == 14, 2014, data35$yr)
data36$yr <- ifelse(data36$yr == 15, 2015, data36$yr)
data37$yr <- ifelse(data37$yr == 16, 2016, data37$yr)
data38$yr <- ifelse(data38$yr == 17, 2017, data38$yr)
data39$yr <- ifelse(data39$yr == 18, 2018, data39$yr)  
data40$yr <- 2019
data41$yr <- 2020
data38$breeder <- as.numeric(data38$breeder)  
data39$breeder <- as.numeric(data39$breeder)
# remove hyphen in 2020 (data41) 
data41 <- data41 %>%
  mutate(sample = as.numeric(gsub("-", "", sample)))



# Loop over the data frames and change column types to correct data type
for (i in 1:41) {
  df_name <- paste0("data", i)
  df <- get(df_name)
  mixed_cols <- sapply(df, function(col) any(!is.na(as.numeric(col))))
  df[, mixed_cols] <- lapply(df[, mixed_cols], as.numeric)
  assign(df_name, df)
}


# create x21 
for (i in 1:41) {
  df_name <- paste0("data", i)
  df <- get(df_name)
  df <- df %>%
    mutate(x21 = NA)
  assign(df_name, df)
}


mixed_cols <- sapply(combined_data, function(col) any(!is.na(as.numeric(col)) & !is.na(as.character(col))))
combined_data <- combined_data %>% mutate_if(mixed_cols, as.numeric)

# Bind the data frames row-wise
data_frames <- lapply(1:41, function(i) get(paste0("data", i))) 
combined_data <- dplyr::bind_rows(data_frames)

write_csv(combined_data, "output/combined data.csv")
write_xlsx(combined_data,"output/combined data.xlsx")



# TESTERS ##########
# Read the first file
first_file <- read_csv(file_names[[1]])
first_column_names <- colnames(first_file)

# Initialize variables 
non_matching_files <- vector("character") # file names 
non_matching_columns <- list() # name of columns 
non_matching_count <- 0  # how many don't match 

# Compare rest of files with 1980 
for (i in 2:length(file_names)) {
  current_file <- read_csv(file_names[[i]])
  current_column_names <- colnames(current_file)
  
  # Check if column names match with the first file
  if (!identical(first_column_names, current_column_names)) {
    non_matching_files <- c(non_matching_files, file_names[i])
    non_matching_count <- non_matching_count + 1
    differing_columns <- setdiff(first_column_names, current_column_names)
    non_matching_columns[[file_names[i]]] <- differing_columns
  }
}

# Print 
if (length(non_matching_files) > 0) {
  message("The following files have differing columns compared to 1980 file:")
  for (file in non_matching_files) {
    message(paste("File:", file))
    message("Differing Columns:")
    if (length(non_matching_columns[[file]]) > 0) {
      for (column in non_matching_columns[[file]]) {
        message(paste("Original Column:", column))
      }
    } else {
      message("None")
    }
    message("---------------")
  }
} else {
  message("All XLSX files have the same column names.")
}

# Show number of files that don't match 
if (non_matching_count > 0) {
  message("Number of files with differing column names compared to the first file: ", non_matching_count)
} else {
  message("All XLSX files have the same column names.")
}


# Rename and Fix 



