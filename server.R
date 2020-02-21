
# use Shiny
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

source("functions.R")


# load data
full_data <- readRDS("data\\data.rds") %>%
  rename_all(tolower) %>%
  mutate(pca_seg = ifelse(pca_seg == "lease", "prior 12M lease", "prior 12M non-lease")) %>%
  filter(pca_label != "E")

leads <- full_data %>% 
  select(ref_dt, party_id, acct_nm, dfs_rsm_full_nm, pca_seg, pca_label) %>% 
  arrange(pca_label)

names(leads) <- c("Date", "Account ID", "Account Name", "DFS Manager", "PCA Segment", "Propensity Label")


# shiny server 
shinyServer(function(input, output) {
  
  # Show Leads Table
  # output$leads_table <- DT::renderDataTable(leads,
  #                                           extensions = 'Buttons',
  #                                           options = list(dom = "Blfrtip",
  #                                                          buttons = list("copy", "pdf"),
  #                                                          pageLength = 20))
  
  output$leads_table <- renderDataTable(leads, options = list(pageLength = 15))
  
  # Reactive values that updates for each new party id (set default values here)
  v <- reactiveValues(color = "light-blue", icon = NULL, model = NULL)
  
  # When action button is triggered...
  observeEvent(input$calculate_recipe, {
    
    # Add progress bar and increase to 0.5
    withProgress(message = 'Please wait',
                 detail = 'Gathering data', value = 0.5,
                 {
                   
                   acct_data <- full_data %>% 
                     filter(party_id == input$lead_party)
                   
                   # Increase progress bar to 0.8  
                   incProgress(0.8, detail = "Storing results")
                   
                   v$data <- acct_data
                   v$model <- "FY18Q1 PCA List"
                   v$mla <- ifelse(v$data$mla_flg == 1, "MLA Customer", "No MLA")
                   v$credit_stat <- paste0("Credit status: ", v$data$cr_app_stat_cd, ", until ", v$data$next_revw_dt)
                   v$credit_lim <- paste0("Credit limit: $", v$data$cr_lim_nbr)
                   v$dell_hist <- paste0("Dell customer: ", v$data$hist_frst_dell_ord_mo_nbr, " months")
                   v$dfs_hist <- paste0("Lease customer: ", v$data$hist_frst_dell_ls_ord_mo_nbr, " months")
                   v$max_cnt <- as.numeric(max(v$data$r36_ls_cntrct_cnt, v$data$r36_dell_tot_ord_cnt, na.rm = TRUE))
                   v$max_amt <- as.numeric(max(v$data$r36_ls_cntrct_amt, v$data$r36_dell_tot_ord_amt, na.rm = TRUE))/1000
                   v$last_dell_ord <- v$data$hist_lst_dell_ord_mo_nbr
                   v$last_sfdc_opp <- v$data$hist_dfs_sfdc_lst_opp_mo_nbr
                   v$last_lease <- v$data$ls_lst_bkng_mo_nbr
                   v$next_lease <- v$data$ls_frst_nxt_term_mo_nbr
                   
                   v$group <- case_when(acct_data$pca_label == "A" ~ "Master Group: Very likely to purchase (80%-100%)",
                                        acct_data$pca_label == "B" ~ "Great to call Group: Likely to purchase (60%-80%)",
                                        acct_data$pca_label == "C" ~ "Good to call Group: Less likely to purchase (40%-60%)",
                                        acct_data$pca_label == "D" ~ "Try to call Group: Not so likely to purchase (10%-40%)")
                   
                   v$color <- case_when(acct_data$pca_label == "A" ~ "green",
                                        acct_data$pca_label == "B" ~ "olive",
                                        acct_data$pca_label == "C" ~ "yellow",
                                        acct_data$pca_label == "D" ~ "orange")
                   
                   v$icon <-  case_when(acct_data$pca_label == "A" ~ "thumbs-up",
                                        acct_data$pca_label == "B" ~ "thumbs-up", 
                                        acct_data$pca_label == "C" ~ "hand-right", 
                                        acct_data$pca_label == "D" ~ "thumbs-down")
                   
                   # Increase progress bar to 1
                   incProgress(1, detail = "Finish")
                 })
  })
  
  
  # PCA model label
  output$pca_label <- renderValueBox({
    valueBox(paste0(v$data$pca_label, " - ", v$data$acct_nm), 
             v$group, 
             icon = icon(v$icon, lib = "glyphicon"),
             color = v$color
    )
  })
  
  
  # Key lead source info
  output$source_info <- renderValueBox({
    valueBox(
      40,
      "Alkalinity", 
      v$model,
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key account info
  output$account_info <- renderValueBox({
    valueBox(
      68,
      "Hardness", 
      v$mla,
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key credit info
  output$credit_info <- renderValueBox({
    valueBox(
      150, 
      "TDS",
      v$credit_lim,
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  # Key historical info
  output$hist_info <- renderValueBox({
    valueBox(
      7.5, 
      "pH", 
      v$dell_hist,
      icon = icon("list"),
      color = "light-blue"
    )
  })
  
  
  
  # Historical purchase plot
  output$hist_purch_plot <- renderPlot({
    if (is.null(v$data)) return()
    else {
      v$data %>% 
        select(ends_with("cnt")) %>% 
        gather(frame, amnt) %>% 
        mutate(fr = str_sub(frame, end = 3),
               fr = recode(fr, r36 = "36 months", r24 = "24 months", r12 = "12 months"),
               fr = factor(fr, levels = c("36 months", "24 months", "12 months")),
               src = ifelse(str_detect(frame, "ls"), "Lease contracts ", "Dell orders "),
               amnt = ifelse(is.na(amnt), 0, amnt)) %>% 
        ggplot(aes(x = fr, y = amnt, fill = src)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = round(amnt)), position = position_dodge2(width = 1), size = 3, vjust = -1) +
        scale_fill_manual(values = c("#007DB8", "#D74324")) +
        ylim(c(0, 1.1 * v$max_cnt)) +  
        ylab("Number of orders/contracts") +
        xlab(" ") +
        theme(legend.title = element_blank(), legend.position = "bottom")
    }
  })
  
  
  # Purchases over time plot
  output$time_purch_plot <- renderPlot({
    if (is.null(v$data)) return()
    else {
      v$data %>% 
        select(ends_with("amt"), -perf_amt) %>%
        gather(frame, amnt) %>%
        mutate(fr = str_sub(frame, end = 3),
               fr = recode(fr, r36 = "36 months", r24 = "24 months", r12 = "12 months"),
               fr = factor(fr, levels = c("36 months", "24 months", "12 months")),
               src = ifelse(str_detect(frame, "ls"), "Lease contracts ", "Dell orders "),
               amnt = as.numeric(ifelse(is.na(amnt), 0, amnt))/1000) %>%
        ggplot(aes(x = fr, y = amnt, fill = src)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = round(amnt)), position = position_dodge2(width = 1), size = 3, vjust = -1) +
        scale_fill_manual(values = c("#007DB8", "#D74324")) +
        ylim(c(0, 1.1 * v$max_amt)) +
        ylab("Purchase amount ($ thousands)") +
        xlab(" ") +
        theme(legend.title = element_blank(), legend.position = "bottom")
    }
  })
  
  
  # output$residuals_mean <- renderText(
  #   if (is.null(v$fitted_values)) "No estimation has been computed, yet"
  #   else paste("Mean:", round(mean(v$residuals),4))
  # )
  # 
  # output$residuals_minmax <- renderUI(
  #   if (is.null(v$fitted_values)) "No estimation has been computed, yet"
  #   else {
  #     str1 <- paste("Min value:", round(min(v$residuals),4))
  #     str2 <- paste("Max value:", round(max(v$residuals),4))
  #     HTML(paste(str1, str2, sep = '<br/>'))
  #   }
  # )
  
  
  # Last Dell order
  output$last_dell <- renderValueBox({
    valueBox(
      v$last_dell_ord, 
      "Months since the latest Dell order", 
      color = "olive"
    )
  })
  
  # Last SFDC opportunity
  output$last_sfdc <- renderValueBox({
    valueBox(
      v$last_sfdc_opp, 
      "Months since the latest DFS SFDC opportunity", 
      color = "olive"
    )
  })
  
  output$last_ls <- renderValueBox({
    valueBox(
      v$last_lease, 
      "Months since the latest booked lease contract", 
      color = "olive"
    )
  })
  
  output$next_ls_exp <- renderValueBox({
    valueBox(
      v$next_lease,
      "Months until the next existing lease contract expires", 
      color = "olive"
    )
  })
  
})