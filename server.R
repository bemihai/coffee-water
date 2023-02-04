
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shinyalert)

source("functions.R")

data <- read_csv("data/raw_water_Romania.csv") %>% 
  chemical_composition()

 
shinyServer(function(input, output) {
  
  val <- reactiveValues(recipe = NULL, alk_color = "light-blue", alk_icon = NULL, hard_color = "light-blue", hard_icon = NULL)
  
  observeEvent(input$calc_recipe, {
    
    if (input$first_coef * input$second_coef * input$third_coef < 0 | 
        input$first_coef %% 100 != input$first_coef |
        input$second_coef %% 100 != input$second_coef |
        input$third_coef %% 100 != input$third_coef) {
      shinyalert("Use only coefficients between 0 and 100.", type = "warning")
    }
    
    if (input$first_coef + input$second_coef + input$third_coef != 100) {
      shinyalert("Coefficients do not sum up to 100.", type = "warning")
    }
    
    val$waters <- c(input$first_water, input$second_water, input$third_water) 
    val$coefs <- c(input$first_coef/100, input$second_coef/100, input$third_coef/100) 
    
    if (length(unique(val$waters)) == length(val$waters)) {
      val$recipe <- create_recipe(data, val$waters, val$coefs)
    } else {
      shinyalert("Choose three different waters.", 
                 "For recipes with two waters, set the coefficient of the third one to zero.", 
                 type = "warning")
    }
    
    val$max_alk <- data %>% 
      filter(Brand %in% val$waters) %>% 
      summarise(alk = max(alkalinity)) %>% 
      pull()
    
    val$max_hard <- data %>% 
      filter(Brand %in% val$waters) %>% 
      summarise(hard = max(hardness)) %>% 
      pull()
    
    val$alk_icon <- case_when(as.logical(val$recipe$alkalinity < 35) ~ "thumbs-down",
                              as.logical(val$recipe$alkalinity < 39) ~ "hand-point-right",
                              as.logical(val$recipe$alkalinity < 41) ~ "thumbs-up",
                              as.logical(val$recipe$alkalinity < 45) ~ "hand-point-right",
                              TRUE ~ "thumbs-down")

    val$alk_color <- case_when(as.logical(val$recipe$alkalinity < 35) ~ "red",
                              as.logical(val$recipe$alkalinity < 39) ~ "yellow",
                              as.logical(val$recipe$alkalinity < 41) ~ "green",
                              as.logical(val$recipe$alkalinity < 45) ~ "yellow",
                              TRUE ~ "red")
    
    val$hard_icon <- case_when(as.logical(val$recipe$hardness < 40) ~ "thumbs-down",
                              as.logical(val$recipe$hardness < 60) ~ "hand-point-right",
                              as.logical(val$recipe$hardness < 80) ~ "thumbs-up",
                              as.logical(val$recipe$hardness < 120) ~ "hand-point-right",
                              TRUE ~ "thumbs-down")
    
    val$hard_color <- case_when(as.logical(val$recipe$hardness < 40) ~ "red",
                               as.logical(val$recipe$hardness < 60) ~ "yellow",
                               as.logical(val$recipe$hardness < 80) ~ "green",
                               as.logical(val$recipe$hardness < 120) ~ "yellow",
                               TRUE ~ "red")

  })
  
  
  output$save_recipe <- downloadHandler(
    filename = function() {
      paste(str_c(val$waters, collapse = ' + '), " - ", str_c(val$coefs, collapse = ' + '), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(val$recipe, file)
    }
  )
  

  output$alkalinity <- renderValueBox({
    valueBox(
      ifelse(val$recipe$alkalinity, round(val$recipe$alkalinity), NULL),
      "Alkalinity", 
      icon = icon(val$alk_icon),
      color = val$alk_color
    )
  })
  

  output$hardness <- renderValueBox({
    valueBox(
      ifelse(val$recipe$hardness, round(val$recipe$hardness), NULL),
      "Hardness", 
      icon = icon(val$hard_icon),
      color = val$hard_color
    )
  })
  

  output$magnesium <- renderValueBox({
    valueBox(
      ifelse(val$recipe$Mg2, round(val$recipe$Mg2), NULL),
      "Magnesium",
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  output$calcium <- renderValueBox({
    valueBox(
      ifelse(val$recipe$Ca2, round(val$recipe$Ca2), NULL), 
      "Calcium", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  
  output$ph <- renderValueBox({
    valueBox(
      ifelse(val$recipe$pH, round(val$recipe$pH, 1), NULL),
      "pH", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  
  output$tds <- renderValueBox({
    valueBox(
      ifelse(val$recipe$TDS, round(val$recipe$TDS), NULL),
      "TDS", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  

  output$plot_ingredients <- renderPlot({
    if (is.null(val$recipe)) return()
    else {
      df <- data %>% 
        filter(Brand %in% val$waters)
      df <- bind_rows(df, df)
      
      data %>% 
        filter(Brand %in% val$waters) %>% 
        plot_waters(1.2*val$max_alk, 1.2*val$max_hard) +
        geom_point(aes(x = val$recipe$alkalinity, y = val$recipe$hardness), color = "red", shape = 18, size = 5) +
        geom_text(aes(x = val$recipe$alkalinity, y = val$recipe$hardness, label="Recipe"), hjust=-0.3, vjust=0, size = 4) +
        geom_path(data = df, aes(x = alkalinity, y = hardness), color = "red", linetype="dotted") 
    }
  })
  
  
  
  output$plot_sca_range <- renderPlot({
    if (is.null(val$recipe)) return()
    else {
      val$recipe %>% 
        plot_recipes() +
        geom_point(aes(x = val$recipe$alkalinity, y = val$recipe$hardness), color = "red", shape = 18, size = 5) +
        geom_text(aes(x = val$recipe$alkalinity, y = val$recipe$hardness, label="Recipe"), hjust=-0.3, vjust=0, size = 4)
    }
  })
  
  
  ###################################################################################
  
  null_recipes <- create_recipe(data, c("Distilled water"), c(1)) %>% 
    filter(Brands != "Distilled water") %>% 
    rename(Alkalinity = alkalinity, Hardness = hardness, Ratio = ratio, `Parts %` = Parts)
  
  recipes <- reactiveValues(recipes = null_recipes)
  
  
  observeEvent(input$find_recipe, {

    # Add progress bar and increase to 0.5
    withProgress(message = 'Please wait', detail = 'Checking data', value = 0.2,
                 {
                   
                   
                   if (input$target_alk * input$min_hard * input$max_hard <= 0) {
                     shinyalert("Invalid alkalinity or hardness.", type = "warning")
                   }
                   
                   if (input$min_hard > input$max_hard) {
                     shinyalert("Hardness values are not correct.", type = "warning")
                   }
                   
                   if (input$target_alk > max(data$alkalinity)) {
                     shinyalert("Alkalinity is too big.", type = "warning")
                   }
                   
                   if (input$min_hard > max(data$hardness) | input$max_hard > max(data$hardness)) {
                     shinyalert("Hardness is too big.", type = "warning")
                   }

                   incProgress(0.5, detail = "Compute recipes with 2 waters")
                   dbl_recipes <- double_recipes(data, input$target_alk, input$min_hard, input$max_hard)
              
                   incProgress(0.8, detail = "Compute recipes with 3 waters")
                   trpl_recipes <- triple_recipes(data, input$target_alk, input$min_hard, input$max_hard)
                   
                   incProgress(1, detail = "Finish")
                   recipes$recipes <- bind_rows(dbl_recipes, trpl_recipes) %>% 
                     rename(
                       Alkalinity = alkalinity, 
                       Hardness = hardness, 
                       Ratio = ratio, 
                       `Parts %` = Parts
                       ) %>% 
                     mutate_if(is.numeric, list(~round(., 1))) %>% 
                     mutate_at(vars(TDS, Alkalinity, Hardness), list(round)) %>% 
                     mutate(
                       Ca2 = round(100*Ca2/Hardness),
                       Mg2 = round(100*Mg2/Hardness)
                       ) %>% 
                     rename(`Ca %` = Ca2, `Mg %` = Mg2) %>% 
                     select(Brands:TDS, Alkalinity, Hardness, Ratio, everything()) %>% 
                     arrange(Hardness)
                 })
  })
  
  
  output$recipe_table <- DT::renderDataTable(recipes$recipes, options = list(dom = "Blfrtip", pageLength = 10))
  
  output$save_found_recipes <- downloadHandler(
    filename = function() {
      paste("alk ", input$target_alk, " ", "hard ", input$min_hard, "-", input$max_hard, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(recipes$recipes, file)
    }
  )
  
  
})



