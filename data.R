library(dplyr)
library(readr)
library(stringr)
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


# create a recipe from a list of different bottled waters with coefficients
create_recipe <- function(data, brands, coefs){
  assert_that(length(brands) == length(coefs),
              msg = 'Number of waters differs from number of coefficients!')
  recipe <- data %>% 
    filter(Brand %in% brands) %>% 
    summarise_if(is.numeric, .fun = ~(.%*%coefs)/sum(coefs)) %>% 
    mutate(
      brands = str_c(brands, collapse = ' + '),
      parts = str_c(coefs, collapse = ' + '),
    ) %>% 
    select(brands, parts, everything()) %>% 
    chemical_composition()
  return(recipe)
}

recipes <- create_recipe(data, c('Borsec'), c(1)) %>% 
  filter(brands == "brands")

# get all recipes with 2 ingredients
for (i in 1:nrow(data)){
  for (j in 1:nrow(data)) {
    if (i != j){
      brands <- c(data[i, 1], data[j, 1])
      for (c in seq(0, 10, 0.5)) {
        coefs <- c(1, c)
        recipe <- create_recipe(data, brands, coefs)
        recipes <- bind_rows(recipes, recipe)
      }
    }
  }
}

write_csv(recipes, 'data/bin_recipes.csv')






  





 




















