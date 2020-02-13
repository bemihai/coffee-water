library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)

source('functions.R')

# bottled water raw data - concentrations in mg/L (ppm)
data <- read_csv('data/raw_water_data.csv') 

double_recipes <- all_double(data)
triple_recipes <- all_triple(data)
recipes<- bind_rows(double_recipes, triple_recipes)


write_csv(recipes, 'data/all_recipes.csv')

recipes %>% 
  filter(
    alkalinity >=39 & alkalinity <=41,
    hardness >= 60 & hardness <= 80,
    ) %>% 
  write_csv('data/best_recipes.csv')




 




















