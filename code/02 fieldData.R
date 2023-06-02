

# Setup ################
library(tidyverse)
library(readxl)


# Load Data ##########
directory_path <- "../data/fiber"
file_names <- list.files(directory_path, pattern = "\\d{4} fibers\\.xlsx$", full.names = TRUE)
# file names has all the xlsx stored in a list 
file_names

# FUNCTIONS ##########

# Read the first file
first_file <- read_excel(file_names[[1]])
first_column_names <- colnames(first_file)

# Initialize variables 
non_matching_files <- vector("character") # file names 
non_matching_columns <- list() # name of columns 
non_matching_count <- 0  # how many don't match 

# Compare rest of files with 1980 
for (i in 2:length(file_names)) {
    current_file <- read_excel(file_names[[i]])
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
  message("The following files have differing columns compared to the first file:")
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




