
# Use ShinyDashboard
library(shinydashboard)

data <- read_csv("data/raw_water_data.csv")
brands <- data$Brand

ui <- dashboardPage(
    
    # Define Header and Sidebar
    dashboardHeader(title = "Water for Coffee",
                    titleWidth = 250),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Manual recipe calculator", tabName = "details", icon = icon("calculator")),
            menuItem("Find best recipes", tabName = "leads", icon = icon("bar-chart"))
        )
    ),
    
    
    dashboardBody(
        
        # Define content for each page
        tabItems(
            
            # Find best recipes
            tabItem(tabName = "leads",
                    h2("Leads"),
                    dataTableOutput("leads_table")
            ),
            
            
            # Manual recipe calculator
            tabItem(tabName = "details",
                    
                    # first row: inputs
                    fluidRow(
                        # Define CSS style for the "Calculate" button
                        tags$head(tags$style(HTML('
                              .estimation_button {background-color: #33CE67; width: 100%}
                              .estimation_button:hover {background-color: #1DAA4C}
                              '))),
                        box(
                            width = 2,
                            title = "First water",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            # input the party id as text
                            selectInput("lead_party", label = NULL, choices = brands, selected = "Distilled water")
                        ),
                        box(
                            width = 2,
                            title = "Second water",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            # input the party id as text
                            selectInput("lead_party", label = NULL, choices = brands, selected = "Distilled water")
                        ),
                        box(
                            width = 2,
                            title = "Third water",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            # input the party id as text
                            selectInput("lead_party", label = NULL, choices = brands, selected = "Distilled water")
                        ),
                        box(
                            width = 2,
                            title = "Fourth water",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            # input the party id as text
                            selectInput("lead_party", label = NULL, choices = brands, selected = "Distilled water")
                        ),
                        
                        box(
                            width = 2,
                            title = "Calculate recipe",
                            status = "success",
                            solidHeader = FALSE,

                            actionButton("calculate_recipe", icon("play"), class = "estimation_button")
                        )
                    ),
                    
                    
                    # second row: recipe key info
                    fluidRow(
                        
                        # Key summary info
                        valueBoxOutput("source_info", width = 2),
                        valueBoxOutput("account_info", width = 2),
                        valueBoxOutput("credit_info", width = 2),
                        valueBoxOutput("hist_info", width = 2)
                    ),
                    
                    # third row: charts
                    fluidRow(
                        
                        # Add box for graph 
                        box(
                            title = "Recipe details",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6,
                            plotOutput("hist_purch_plot")
                        ),
                        
                        # Add box for graph 
                        box(
                            title = "Compare to SCA standards",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 4,
                            plotOutput("time_purch_plot")
                        )
                    )
                    
            )
        )
    )
)




