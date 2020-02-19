library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)


# compute chemical composition summary of a water
chemical_composition <- function(df){
  df <- df %>% 
    mutate(
      alkalinity = 0.8202 * HCO3,                                             # total alkalinity (ppm as CaCO3)
      Ca2 = 2.497 * Ca,                                                       # Calcium hardness (ppm as CaCO3)         
      Mg2 = 4.118 * Mg,                                                       # Magnesium hardness (ppm as CaCO3)
      hardness = Ca2 + Mg2,                                                   # total hardness (ppm as CaCO3)
      ratio = hardness/alkalinity,                                         # hardness/alkalinity ratio
      ratio = if_else(!is.na(ratio), ratio, 0)
    )
  return(df)
}


# filter waters within SCA accepted ranges
sca_filter <- function(water){
  alk  <- water['alkalinity'] >= 20 & water['alkalinity'] <= 60
  hard <- water['hardness'] >= 40 & water['hardness'] <= 100
  return(alk & hard)
}



# create a recipe from a list of different bottled waters with coefficients
create_recipe <- function(data, brands, coefs){
  assert_that(length(brands) == length(coefs),
              msg = 'Number of waters differs from number of coefficients!')
  recipe <- data %>% 
    filter(Brand %in% brands) %>% 
    mutate(Brand = factor(Brand, ordered = TRUE, levels = brands)) %>% 
    arrange(Brand) %>% 
    summarise_if(is.numeric, .fun = ~(. %*% coefs)/sum(coefs)) %>% 
    mutate(
      brands = str_c(brands, collapse = ' + '),
      parts = str_c(coefs, collapse = ' + '),
    ) %>% 
    select(brands, parts, everything()) %>% 
    chemical_composition()
  return(recipe)
}


plot_water <- function(df) {
  
  diag <- data.frame(a = c(0, 320)) 
  
  df %>% 
    ggplot() + 
    geom_segment(aes(x = 40, y = 17, xend = 40, yend = 85), colour = "green", size = 3.5) +
    geom_point(aes(x = 40, y = 68), color = "red", shape = 18, size = 3) +
    geom_point(aes(x = alkalinity, y = hardness), shape = 18, size = 2.5) +
    geom_text(aes(x = alkalinity, y = hardness, label=Brand),hjust=0, vjust=0, size = 3) +
    geom_line(data = diag, aes(x = a, y = a), linetype="dotted", color = "red") +
    scale_x_continuous(name = "Alkalinity (ppm CaCO3)", limits = c(0, 320), breaks = seq(0, 320, 20)) + 
    scale_y_continuous(name = "Total Hardness (ppm CaCO3)", limits = c(0, 340), breaks = seq(0, 340, 20))
}


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
    scale_x_continuous(name = "Alkalinity (ppm CaCO3)", limits = c(0, 220), breaks = seq(0, 220, 20)) + 
    scale_y_continuous(name = "Total Hardness (ppm CaCO3)", limits = c(0, 340), breaks = seq(0, 340, 20)) +
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

































# # get all recipes with 3 ingredients
# all_triple <- function(data) {
#   
#   triple_recipes <- create_recipe(data, c('Distilled water'), c(1)) %>% 
#     filter(brands == "brands")
#   
#   for (i in 1:nrow(data)){
#     for (j in 1:nrow(data)) {
#       for (k in 1:nrow(data)) {
#         if ((i != j) & (j!= k) & (k != i)){
#           brands <- c(data[i, 1], data[j, 1], data[k, 1])
#           for (c1 in seq(0.25, 5, 0.25)) {
#             for (c2 in seq(0.25, 5, 0.25)) {
#               coefs <- c(1, c1, c2)
#               recipe <- create_recipe(data, brands, coefs)
#               if (sca_filter(recipe)) {
#                 triple_recipes <- bind_rows(triple_recipes, recipe)
#               }
#             }
#           }
#         }
#       }
#     }
#   }
#   
#   return(triple_recipes)
# }


# # get all recipes with 2 ingredients
# all_double <- function(data) {
#   
#   double_recipes <- create_recipe(data, c('Distilled water'), c(1)) %>% 
#     filter(brands == "brands")
#   
#   
#   for (i in 1:nrow(data)){
#     for (j in 1:nrow(data)) {
#       if (i != j){
#         brands <- c(data[i, 1], data[j, 1])
#         for (c in seq(0.25, 10, 0.25)) {
#           coefs <- c(1, c)
#           recipe <- create_recipe(data, brands, coefs)
#           if (sca_filter(recipe)) {
#             double_recipes <- bind_rows(double_recipes, recipe)
#           }
#         }
#       }
#     }
#   }
#   
#   return(double_recipes)
# }







