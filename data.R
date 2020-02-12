library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)

# bottled water raw data - concentrations in mg/L (ppm)
data <- read_csv('data/raw_water_data.csv') 

# compute chemical composition summary
chemical_composition <- function(df){
  df <- df %>% 
    mutate(
      alkalinity = 0.8202 * HCO3,                                             # total alkalinity (ppm as CaCO3)
      Ca2 = 2.497 * Ca,                                                       # Calcium hardness (ppm as CaCO3)         
      Mg2 = 4.118 * Mg,                                                       # Magnesium hardness (ppm as CaCO3)
      hardness = Ca2 + Mg2,                                                   # total hardness (ppm as CaCO3)
      bica_ratio = if_else(!is.na(HCO3/Ca), HCO3/Ca, 0),                      # bicarbonates/Calcium ratio 
      ha_ratio = hardness/alkalinity,                                         # hardness/alkalinity ratio
      ha_ratio = if_else(!is.na(ha_ratio), ha_ratio, 0)
    )
  return(df)
}

# define SCA accepted ranges
sca_filter <- function(water){
  alk  <- water['alkalinity'] >= 20 & water['alkalinity'] <= 80
  hard <- water['hardness'] >= 20 & water['hardness'] <= 200
  ph   <- water['pH'] >= 6.5 & water['pH'] <= 7.5
  tds  <- water['TDS'] >= 50 & water['TDS'] <= 180
  return(alk & hard & ph & tds)
}


# create a recipe from a list of different bottled waters with coefficients
create_recipe <- function(data, brands, coefs){
  assert_that(length(brands) == length(coefs),
              msg = 'Number of waters differs from number of coefficients!')
  recipe <- data %>% 
    filter(Brand %in% brands) %>% 
    summarise_if(is.numeric, .fun = ~(. %*% coefs)/sum(coefs)) %>% 
    mutate(
      brands = str_c(brands, collapse = ' + '),
      parts = str_c(coefs, collapse = ' + '),
    ) %>% 
    select(brands, parts, everything()) %>% 
    chemical_composition()
  return(recipe)
}


double_recipes <- create_recipe(data, c('Borsec'), c(1)) %>% 
  filter(brands == "brands")

# get all recipes with 2 ingredients
for (i in 1:nrow(data)){
  for (j in 1:nrow(data)) {
    if (i != j){
      brands <- c(data[i, 1], data[j, 1])
      for (c in seq(0.5, 10, 0.5)) {
        coefs <- c(1, c)
        recipe <- create_recipe(data, brands, coefs)
        if (sca_filter(recipe)) {
          double_recipes <- bind_rows(double_recipes, recipe)
        }
      }
    }
  }
}


triple_recipes <- create_recipe(data, c('Borsec'), c(1)) %>% 
  filter(brands == "brands")

# get all recipes with 3 ingredients
for (i in 1:nrow(data)){
  for (j in 1:nrow(data)) {
    for (k in 1:nrow(data)) {
      if ((i != j) & (j!= k) & (k != i)){
        brands <- c(data[i, 1], data[j, 1], data[k, 1])
        for (c1 in seq(0.5, 6, 0.5)) {
          for (c2 in seq(0.5, 6, 0.5)) {
            coefs <- c(1, c1, c2)
            recipe <- create_recipe(data, brands, coefs)
            if (sca_filter(recipe)) {
              triple_recipes <- bind_rows(triple_recipes, recipe)
            }
          }
        }
      }
    }
  }
}


all_recipes<- bind_rows(double_recipes, triple_recipes)
write_csv(all_recipes, 'data/all_recipes.csv')


# plot recipes against SCA and CDH ranges
plot_recipe <- function(df) {
  
  chd_ideal <- data.frame(alkalinity = c(38, 40,  41,  50,  75,  71,  69,  60, 50, 38), 
                          hardness   = c(50, 140, 160, 170, 175, 140, 120, 80, 60, 50))
  ratio <- data.frame(alkalinity = c(10, 20, 40, 60, 80, 100, 120)) %>% 
    mutate(hardness = alkalinity * 1.8)
    
  df %>% 
    ggplot() + 
    geom_segment(aes(x = 40, y = 17, xend = 40, yend = 85), colour = "green", size = 3) +
    geom_point(aes(x = alkalinity, y = hardness), shape = 1, size = 2.5) +
    geom_line(data = ratio, aes(x = alkalinity, y = hardness), linetype="dotted") +
    geom_path(data = chd_ideal, aes(x = alkalinity, y = hardness), color = "red", size = 1) +
    scale_x_continuous(name = "Alkalinity (ppm CaCO3)", limits = c(0, 120), breaks = seq(0, 120, 20)) + 
    scale_y_continuous(name = "Total Hardness (ppm CaCO3)", limits = c(0, 240), breaks = seq(0, 240, 20)) +
    annotate("text", x = 15, y = 0, label = "weak, sour, sharp") +
    annotate("text", x = 15, y = 10, label = "under-extracted") +
    annotate("text", x = 105, y = 0, label = "weak, chalky, flat") +
    annotate("text", x = 105, y = 10, label = "under-extracted") +
    annotate("text", x = 15, y = 230, label = "heavy, dull, sour") +
    annotate("text", x = 15, y = 240, label = "over-extracted") +
    annotate("text", x = 105, y = 230, label = "weak, chalky, flat") +
    annotate("text", x = 105, y = 240, label = "over-extracted") +
    annotate("text", x = 57, y = 22, label = "SCA Standard", size = 4) +
    annotate("text", x = 47, y = 180, label = "CDH Ideal Zone", size = 4)
}
   

  
  





 




















