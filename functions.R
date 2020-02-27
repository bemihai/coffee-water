library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(assertthat)

source('geometry.R')


# compute chemical composition summary of a water
chemical_composition <- function(df){
  df <- df %>% 
    mutate(
      alkalinity = 0.8202 * HCO3,                                             # total alkalinity (ppm as CaCO3)
      Ca2 = 2.497 * Ca,                                                       # Calcium hardness (ppm as CaCO3)         
      Mg2 = 4.118 * Mg,                                                       # Magnesium hardness (ppm as CaCO3)
      hardness = Ca2 + Mg2,                                                   # total hardness (ppm as CaCO3)
      ratio = hardness/alkalinity,                                            # hardness/alkalinity ratio
      ratio = if_else(!is.na(ratio), ratio, 0)
    )
  return(df)
}


# create a recipe from a list of different bottled waters with coefficients
create_recipe <- function(data, brands, coefs){
  assert_that(length(brands) == length(coefs),
              msg = 'Number of waters differs from number of coefficients!')
  assert_that(length(unique(brands)) == length(brands),
              msg = 'Choose different waters!')
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


# find double recipes with given alkalinity and hardness in a certain range
double_recipes <- function(data, target, h_min, h_max) {
  
  recipes <- create_recipe(data, c("Distilled water"), c(1)) %>% 
    filter(brands != "Distilled water")
  
  for (i in 1:(nrow(data)-1)) {
    for (j in (i+1):(nrow(data))) {
      A <- c(data$alkalinity[i], data$hardness[i])
      B <- c(data$alkalinity[j], data$hardness[j])
      # check if there is a recipe like this
      if (is_segment_sca(A, B, target)) {
        # compute coordinates of the intersection point
        t <- compute_segment_sca(A, B, target)
        coord <- compute_segment_coord(A, B, t)
        # check if hardness is inside the range
        if (coord[2] >= h_min & coord[2] <= h_max) {
          t <- round(t, 2)
          recipe <- create_recipe(data, c(data$Brand[i], data$Brand[j]), c(t, 1-t))
          recipes <- bind_rows(recipes, recipe)
        }
      }
    }
  }
  return(recipes)
}


# find double recipes with given alkalinity and hardness in a certain range
triple_recipes <- function(data, target, h_min, h_max) {
  
  recipes <- create_recipe(data, c("Distilled water"), c(1)) %>% 
    filter(brands != "Distilled water")
  
  for (i in 1:(nrow(data)-2)) {
    for (j in (i+1):(nrow(data)-1)) {
      for (k in (j+1):(nrow(data))) {
        
        A <- c(data$alkalinity[i], data$hardness[i])
        B <- c(data$alkalinity[j], data$hardness[j])
        C <- c(data$alkalinity[k], data$hardness[k])
        
        # check if the triangle is not trivial (area > 2000)
        if (is_triangle(A, B, C, 2000)) {
          # check if there is a recipe like this
          if (is_triangle_sca(A, B, C, target)) {
            # compute coordinates of the intersection segment
            segment <- compute_triangle_sca(A, B, C, target)
            # choose 3 hardness values on the segment 
            sh_min <- min(segment[2], segment[4])
            sh_max <- max(segment[2], segment[4])
            h_values <- seq(sh_min, sh_max, length.out = 5)[2:4]
            # keep only those inside the range
            h_values <- h_values[h_values >= h_min & h_values <= h_max]
            # for each value, compute baricentic coordinates 
            for (h_val in h_values) {
              b <- compute_baricentric(A, B, C, c(target, h_val))
              # filter contribution > 5%
              if (min(b)>0.05) {
                b1 <- round(b[1], 2)
                b2 <- round(b[2], 2)
                b <- c(b1, b2, round(1 - b1 - b2, 2))
                # create recipe
                recipe <- create_recipe(data, c(data$Brand[i], data$Brand[j], data$Brand[k]), b)
                recipes <- bind_rows(recipes, recipe)
              }
            }
          }
        }
      }
    }
  }
  return(recipes)
}


plot_waters <- function(df, max_alk, max_hard) {
  
  plot <- df %>% 
    ggplot() + 
    geom_point(aes(x = alkalinity, y = hardness), shape = 18, size = 2.5) +
    geom_text(aes(x = alkalinity, y = hardness, label=Brand),hjust=-0.1, vjust=0, size = 3) +
    scale_x_continuous(name = "Alkalinity (ppm CaCO3)", limits = c(0, max_alk), breaks = seq(0, max_alk, 10)) + 
    scale_y_continuous(name = "Total Hardness (ppm CaCO3)", limits = c(0, max_hard), breaks = seq(0, max_hard, 20))
  
  return(plot)
}


# plot recipes against SCA and CDH ranges
plot_recipes <- function(df) {
  
  chd_ideal <- data.frame(alkalinity = c(38, 40,  41,  50,  75,  71,  69,  60, 50, 38), 
                          hardness   = c(50, 140, 160, 170, 175, 140, 120, 80, 60, 50))
  
  ratio <- data.frame(alkalinity = c(10, 20, 40, 60, 80, 100, 120)) %>% 
    mutate(hardness = alkalinity * 1.8)
  
  plot <- df %>% 
    ggplot() + 
    geom_segment(aes(x = 40, y = 17, xend = 40, yend = 85), colour = "green", size = 3) +
    geom_point(aes(x = alkalinity, y = hardness), shape = 1, size = 2.5) +
    geom_line(data = ratio, aes(x = alkalinity, y = hardness), linetype="dotted") +
    geom_path(data = chd_ideal, aes(x = alkalinity, y = hardness), color = "red", size = 1) +
    scale_x_continuous(name = "Alkalinity (ppm CaCO3)", limits = c(0, 120), breaks = seq(0, 120, 10)) + 
    scale_y_continuous(name = "Total Hardness (ppm CaCO3)", limits = c(0, 240), breaks = seq(0, 240, 20)) +
    annotate("text", x = 10, y = 0, label = "weak, sour, sharp") +
    annotate("text", x = 10, y = 10, label = "under-extracted") +
    annotate("text", x = 115, y = 0, label = "weak, chalky, flat") +
    annotate("text", x = 115, y = 10, label = "under-extracted") +
    annotate("text", x = 10, y = 230, label = "heavy, dull, sour") +
    annotate("text", x = 10, y = 240, label = "over-extracted") +
    annotate("text", x = 115, y = 230, label = "weak, chalky, flat") +
    annotate("text", x = 115, y = 240, label = "over-extracted") +
    annotate("text", x = 50, y = 22, label = "SCA Standard", size = 4) +
    annotate("text", x = 47, y = 180, label = "CDH Ideal Zone", size = 4)
  
  return(plot)
}









































