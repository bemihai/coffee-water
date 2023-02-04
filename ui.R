
library(shinyalert)
library(shinydashboard)
library(dplyr)
library(readr)

# xml2::write_html(rvest::html_node(xml2::read_html("details.html"), "body"), file = "details_fixed.html")

brands <- read_csv("data/raw_water_Romania.csv") %>% 
    pull(Brand)


ui <- dashboardPage(
    
    dashboardHeader(title = "Water for Coffee", titleWidth = 230),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Calculator", tabName = "manual_calc", icon = icon("calculator")),
            menuItem("Find waters", tabName = "target_calc", icon = icon("bar-chart"))
            # menuItem("Details", tabName = "details", icon = icon("info"))
        )
    ),
    
    dashboardBody(
        
        useShinyalert(),
        
        tabItems(
            
            tabItem(tabName = "manual_calc",
                    h2("Waters calculator"),
                    h4("Select bottled waters from the list and choose proportions (they must sum up to 100%)."),
                    br(),
                    
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
                            selectInput("first_water", label = NULL, choices = brands, selected = "Smart Water"),
                            numericInput("first_coef", label = "Proportion %", 20, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            title = "Second water",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("second_water", label = NULL, choices = brands, selected = "Bucovina"),
                            numericInput("second_coef", label = "Proportion %", 50, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            title = "Third water",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("third_water", label = NULL, choices = brands, selected = "Izvorul Minunilor"),
                            numericInput("third_coef", label = "Proportion %", 30, min = 0, max = 100, step = 1)
                        ),
                        
                        box(
                            width = 3,
                            status = "success",
                            solidHeader = TRUE,
                            actionButton("calc_recipe", icon("calculator"), label =  "Calculate recipe",  class = "estimation_button"),
                            downloadButton("save_recipe", "Save recipe to CSV", class = "estimation_button")
                            
                        )
                    ),
                    
                    
                    fluidRow(
                        
                        valueBoxOutput("alkalinity", width = 2),
                        valueBoxOutput("hardness", width = 2),
                        valueBoxOutput("calcium", width = 2),
                        valueBoxOutput("magnesium", width = 2),
                        valueBoxOutput("tds", width = 2),
                        valueBoxOutput("ph", width = 2)
                    ),
                    
                    
                    fluidRow(
                        
                        box(
                            title = "Compare to starting waters",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6,
                            plotOutput("plot_ingredients")
                        ),
                        
                        box(
                            title = "Compare to SCA standards and CDH Ideal Zone",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6,
                            plotOutput("plot_sca_range")
                        )
                    )
            ),
            

            tabItem(
                tabName = "target_calc",
                h2("Waters with specific alkalinity and hardness"),
                br(),
                
                fluidRow(
                
                    box(
                        width = 3,
                        title = "Alkalinity",
                        status = "primary",
                        solidHeader = TRUE,
                        numericInput("target_alk", label = NULL, 40, min = 20, max = 100, step = 5)
                    ),
                
                    box(
                        width = 3,
                        title = "Min Hardness",
                        status = "primary",
                        solidHeader = TRUE,
                        numericInput("min_hard", label = NULL, 68, min = 0, max = 220, step = 10)
                    ),
                    
                    box(
                        width = 3,
                        title = "Max Hardness",
                        status = "primary",
                        solidHeader = TRUE,
                        numericInput("max_hard", label = NULL, 70, min = 0, max = 220, step = 12)
                    ),
                        
                    box(
                        width = 3,
                        status = "success",
                        solidHeader = TRUE,
                        actionButton("find_recipe", icon = icon("search"), label =  "Find recipes",  class = "estimation_button"),
                        downloadButton("save_found_recipes", "Save recipes to CSV", class = "estimation_button")
                        
                    )
                ),
                
                
                fluidRow(
                    
                    box(
                        width = 12,
                        DT::DTOutput("recipe_table")
                    )
                )
        
            )
            
            # tabItem(
            #     
            #     tabName = "details",
            #     includeHTML("details_fixed.html")
            # )
            
        )
    )
)




