library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)

source('functions.R')

# bottled water raw data - concentrations in mg/L (ppm)
data <- read_csv('data/raw_water_data.csv') 


# saveRDS(recipes, "data\\recipes.rds")
data %>% 
  chemical_composition() %>% 
  plot_water()































