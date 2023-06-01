---
  title: "01 eda"
format: html
editor: visual
---
  
# Setup
library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)
library(USAboundaries)
library(USAboundariesData)

coords <- read_excel("../data/Cotton heatress Longitde and Latitude.xlsx")

coords 
print(coords[30:45, ])

# Wrangling ########################################
coords_w <- coords |>
  # Standardizing names
  clean_names() |>
  # Select target columns  
  dplyr::select(year, reg_code, loc, loccode, varcode, latitude, longitude) |>
  mutate(latitude = gsub("° N", "", as.character(latitude)),
         longitude = gsub("° W", "", as.character(longitude)),
         loc = gsub("AZ", "AK", as.character(loc))) |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

## View entire decimal value  ########
options(digits =6)
print(as.data.frame(coords_w[30:45, ]))

## Transform into geospatial object #################
# remove NA and * -1 
test <- coords_w |>
  drop_na(latitude, longitude) |> 
  #   filter(longitude > 130) %>% # to test AZ point 
  mutate(longitude = longitude *-1)

# EDA ###############
# Will check data with tabular summaries and graphs. - `summary()` - `ggplot()`
## Plotting on state boundaries graph ###########
states_contemporary <- us_states()

# save plot to coord_plot object 
coord_plot <- ggplot()+
  geom_point(data = test, aes(x = longitude, y = latitude))+
  geom_sf(data = states_contemporary, fill = NA)

coord_plot  
# Export to file #################
# use ggsave() (svg, png, pdf)
ggsave(path = "../figs", "coords_plot.png", plot=coord_plot) 


