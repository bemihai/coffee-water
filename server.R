
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

source("functions.R")

data <- read_csv("data/raw_water_data.csv") %>%
  chemical_composition()


# shiny server 
shinyServer(function(input, output) {
  
  output$leads_table <- DT::renderDataTable(data,
                                            extensions = 'Buttons',
                                            options = list(dom = "Blfrtip",
                                                           buttons = list("excel", "csv"),
                                                           pageLength = 15))
  
  # output$leads_table <- renderDataTable(data, options = list(pageLength = 15))
  
  # Reactive values that updates for each new party id (set default values here)
  v <- reactiveValues(color = "light-blue", icon = NULL, model = NULL)
  
  # # When action button is triggered...
  # observeEvent(input$calc_recipe, {
  #   
  #   # Add progress bar and increase to 0.5
  #   withProgress(message = 'Please wait', detail = 'Gathering data', value = 0.5,
  #                {
  #                  
  #                  acct_data <- full_data %>% 
  #                    filter(party_id == input$lead_party)
  #                  
  #                  # Increase progress bar to 0.8  
  #                  incProgress(0.8, detail = "Storing results")
  #                  
  #                  v$data <- acct_data
  #                  v$model <- "FY18Q1 PCA List"
  #                  v$mla <- ifelse(v$data$mla_flg == 1, "MLA Customer", "No MLA")
  #                  v$credit_stat <- paste0("Credit status: ", v$data$cr_app_stat_cd, ", until ", v$data$next_revw_dt)
  #                  v$credit_lim <- paste0("Credit limit: $", v$data$cr_lim_nbr)
  #                  v$dell_hist <- paste0("Dell customer: ", v$data$hist_frst_dell_ord_mo_nbr, " months")
  #                  v$dfs_hist <- paste0("Lease customer: ", v$data$hist_frst_dell_ls_ord_mo_nbr, " months")
  #                  v$max_cnt <- as.numeric(max(v$data$r36_ls_cntrct_cnt, v$data$r36_dell_tot_ord_cnt, na.rm = TRUE))
  #                  v$max_amt <- as.numeric(max(v$data$r36_ls_cntrct_amt, v$data$r36_dell_tot_ord_amt, na.rm = TRUE))/1000
  #                  v$last_dell_ord <- v$data$hist_lst_dell_ord_mo_nbr
  #                  v$last_sfdc_opp <- v$data$hist_dfs_sfdc_lst_opp_mo_nbr
  #                  v$last_lease <- v$data$ls_lst_bkng_mo_nbr
  #                  v$next_lease <- v$data$ls_frst_nxt_term_mo_nbr
  #                  
  #                  v$group <- case_when(acct_data$pca_label == "A" ~ "Master Group: Very likely to purchase (80%-100%)",
  #                                       acct_data$pca_label == "B" ~ "Great to call Group: Likely to purchase (60%-80%)",
  #                                       acct_data$pca_label == "C" ~ "Good to call Group: Less likely to purchase (40%-60%)",
  #                                       acct_data$pca_label == "D" ~ "Try to call Group: Not so likely to purchase (10%-40%)")
  #                  
  #                  v$color <- case_when(acct_data$pca_label == "A" ~ "green",
  #                                       acct_data$pca_label == "B" ~ "olive",
  #                                       acct_data$pca_label == "C" ~ "yellow",
  #                                       acct_data$pca_label == "D" ~ "orange")
  #                  
  #                  v$icon <-  case_when(acct_data$pca_label == "A" ~ "thumbs-up",
  #                                       acct_data$pca_label == "B" ~ "thumbs-up", 
  #                                       acct_data$pca_label == "C" ~ "hand-right", 
  #                                       acct_data$pca_label == "D" ~ "thumbs-down")
  #                  
  #                  # Increase progress bar to 1
  #                  incProgress(1, detail = "Finish")
  #                })
  # })
  
  
  
  
  # Key lead source info
  output$alkalinity <- renderValueBox({
    valueBox(
      40,
      "Alkalinity", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key account info
  output$hardness <- renderValueBox({
    valueBox(
      68,
      "Hardness", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key credit info
  output$magnesium <- renderValueBox({
    valueBox(
      26, 
      "Magnesium",
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key historical info
  output$calcium <- renderValueBox({
    valueBox(
      43, 
      "Calcium", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  output$ph <- renderValueBox({
    valueBox(
      7, 
      "pH", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  output$tds <- renderValueBox({
    valueBox(
      180, 
      "TDS", 
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  
  
  # Historical purchase plot
  output$plot_ingredients <- renderPlot({
    if (is.null(data)) return()
    else {
      data %>% 
        plot_water()
    }
  })
  
  
  # Purchases over time plot
  output$plot_sca_range <- renderPlot({
    if (is.null(data)) return()
    else {
      data %>% 
        plot_water()
    }
  })
  
  
})