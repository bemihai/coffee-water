

library(shinydashboard)
library(dplyr)
library(readr)

brands <- read_csv("data/raw_water_data.csv") %>% 
    pull(Brand)


ui <- dashboardPage(
    
    dashboardHeader(title = "Water for Coffee", titleWidth = 230),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Manual recipe calculator", tabName = "manual_calc", icon = icon("calculator")),
            menuItem("Find target recipes", tabName = "target_calc", icon = icon("bar-chart"))
        )
    ),
    
    
    dashboardBody(
        
        tabItems(
            
            tabItem(tabName = "manual_calc",
                    h2("Manual recipe calculator"),
                    br(),
                    h4("Select bottled waters from the dropdown menu and input their proportions."),
                    
                    fluidRow(
                        
                        tags$head(tags$style(HTML('
                              .estimation_button {background-color: #33CE67; width: 100%}
                              .estimation_button:hover {background-color: #1DAA4C}
                              '))),
                        
                        box(
                            width = 3,
                            title = "First water",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("first_water", label = NULL, choices = brands, selected = "Distilled water"),
                            numericInput("first_coef", label = "Proportion %", 40, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            title = "Second water",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("second_water", label = NULL, choices = brands, selected = "Distilled water"),
                            numericInput("second_coef", label = "Proportion %", 40, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            title = "Third water",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("third_water", label = NULL, choices = brands, selected = "Distilled water"),
                            numericInput("third_coef", label = "Proportion %", 20, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            status = "success",
                            solidHeader = TRUE,
                            actionButton("calc_recipe", label =  "Calculate recipe",  class = "estimation_button"),
                            actionButton("save_recipe", label =  "Save to CSV", class = "estimation_button")
                            
                        )
                    ),
                    

                    fluidRow(
                        
                        valueBoxOutput("alkalinity", width = 2),
                        valueBoxOutput("hardness", width = 2),
                        valueBoxOutput("magnesium", width = 2),
                        valueBoxOutput("calcium", width = 2),
                        valueBoxOutput("tds", width = 2),
                        valueBoxOutput("ph", width = 2)
                    ),
                    
                    
                    fluidRow(
                        
                        box(
                            title = "Compare to ingredients",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6,
                            plotOutput("plot_ingredients")
                        ),
                        
                        box(
                            title = "Compare to SCA standards",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6,
                            plotOutput("plot_sca_range")
                        )
                    )
            ),
            

            tabItem(tabName = "target_calc",
                    h2("Recipes with specific alkalinity and hardness"),
                    br(),
                    
                    fluidRow(
                        
                        box(
                            width = 3,
                            title = "Alkalinity",
                            status = "primary",
                            solidHeader = TRUE,
                            numericInput("target_alk", label = NULL, 40, min = 0, max = 120, step = 10)
                        ),
                        
                        box(
                            width = 3,
                            title = "Hardness",
                            status = "primary",
                            solidHeader = TRUE,
                            numericInput("target_hard", label = NULL, 68, min = 0, max = 220, step = 10)
                        ),
                        
                        box(
                            width = 3,
                            status = "success",
                            solidHeader = TRUE,
                            actionButton("calc_recipe", label =  "Find recipes",  class = "estimation_button")
                            
                        )
                    ),
                    
                    
                    fluidRow(
                        box(
                            width = 9,
                            DT::DTOutput("leads_table")
                        )
                    )
        
            )
        )
    )
)




