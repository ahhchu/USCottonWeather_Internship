---
  title: "01 eda"
format: html
editor: visual
---
  
  # Setup
  
install.packages('tidyverse')
install.packages('readxl')
install.packages('janitor')
install.packages('sf')
install.packages('stringr')



library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(stringr)
library(readr)
library(dplyr)



coords <- read_excel("../data/Cotton heatress Longitde and Latitude.xlsx")

coords 
print(coords[30:45, ])

# Wrangling
coords_w <- coords |>
  # Standardizing names
  clean_names() |>
  # Select target columns  
  dplyr::select(year, reg_code,
                loccode, varcode,
                latitude, longitude 
  )
# Remove degree N and W from latitude and longitude
coords_w$latitude<-gsub("° N","",as.character(coords_w$latitude))
coords_w$longitude<-gsub("° W","",as.character(coords_w$longitude))


# Transform into numeric
coords_w$latitude <- as.numeric(coords_w$latitude)
coords_w$longitude <- as.numeric(coords_w$longitude)

# To see the entire value and not just rounded format 
options(digits =6)
print(as.data.frame(coords_w[30:45, ])) # these rows show values that had N/W
# Transform into geospatial object

coords_w

# EDA

# Will check data with tabular summaries and graphs. - `summary()` - `ggplot()`

# Export to file
