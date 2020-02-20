library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)

source('functions.R')

# bottled water raw data - concentrations in mg/L (ppm)
data <- read_csv('data/raw_water_data.csv')  %>% 
  chemical_composition()


# saveRDS(recipes, "data\\recipes.rds")
data %>% 
  plot_water()


dbl_recipes <- double_recipes(data, 40, 40, 150)
trpl_recipes <- triple_recipes(data, 40, 40, 150)

dbl_recipes %>% 
  plot_recipe()

trpl_recipes %>% 
  plot_recipe()


bind_rows(dbl_recipes, trpl_recipes) %>% 
  write_csv('recipes/recipes.csv')




















