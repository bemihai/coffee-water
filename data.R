library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

source('functions.R')


# bottled water raw data - concentrations in mg/L (ppm)
# data <- read_csv('data/raw_water_Romania.csv')  %>% 
#   chemical_composition()

data <- read_csv('data/full_water_data.csv')  %>% 
  chemical_composition()


diag <- data.frame(a = c(0, 320))
# 
# # saveRDS(recipes, "data\\recipes.rds")

data %>%
  plot_waters(320, 380) +
  geom_segment(aes(x = 40, y = 17, xend = 40, yend = 85), colour = "green", alpha = 0.05, size = 3.5) +
  geom_point(aes(x = 40, y = 68), color = "red", shape = 18, size = 3) +
  geom_line(data = diag, aes(x = a, y = a), linetype="dotted", color = "red")

# 
# dbl_recipes <- double_recipes(data, 40, 40, 150)
# trpl_recipes <- triple_recipes(data, 40, 40, 150)
# 
# dbl_recipes %>%
#   plot_recipes()
# 
# trpl_recipes %>%
#   plot_recipes()
# 
# 
# bind_rows(double_recipes(data, 40, 50, 150), triple_recipes(data, 40, 50, 150)) %>%
#   write_csv('recipes/recipes.csv')
