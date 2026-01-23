library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(bslib)
library(stringr)
library(tidyr)
 
# Data preprocessing
covid <- read_csv("../../flatten-covid-19-survey-data-on-symptoms-demographics-and-mental-health-in-canada-1.0/Flatten_Data/schema_3_ontario_final.csv") %>%
  clean_names() %>%
  mutate(
    across(c(probable, vulnerable, fever_chills_shakes, cough, shortness_of_breath,
             travel_outside_canada, contact_with_illness, tested),
           ~ factor(., levels = c("n", "y"), labels = c("No", "Yes"))),
    any_medical_conditions = factor(any_medical_conditions, levels = "y", labels = "Yes"),
    across(c(contact_in_household, self_isolating),
           ~ factor(., levels = c("n", "y", "NA"), labels = c("No", "Yes", "NA"))),
    tobacco_usage = factor(na_if(tobacco_usage, "NA"), levels = c("n", "y", "quitSmoking"), labels = c("No", "Yes", "quit")),
    covid_positive = factor(na_if(covid_positive, "n")),
    vuln_and_pos = vulnerable == "y" & covid_positive == "positively",
    vuln_and_pos = factor(replace_na(vulnerable == "Yes" & covid_positive == "positively", FALSE), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    
    fcs_and_cough = factor(replace_na(fever_chills_shakes == "Yes" & cough == "Yes", FALSE), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    fcs_sob = factor(replace_na(fever_chills_shakes == "Yes" & shortness_of_breath == "Yes", FALSE), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    cough_sob = factor(replace_na(shortness_of_breath == "Yes" & cough == "Yes", FALSE), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    allsigns = factor(replace_na(fever_chills_shakes == "Yes" & cough == "Yes" & shortness_of_breath == "Yes", FALSE), levels = c(FALSE, TRUE), labels = c("No", "Yes")),
    mental_health_impact = factor(na_if(mental_health_impact, "NA")),
    sex = factor(na_if(sex, "na"), levels = c("f", "m", "NA"), labels = c("Female", "Male", "Not Specified")),
    age_1 = factor(age_1, levels = c("<26", "26-44", "45-64", ">65"), ordered = TRUE),
    month = factor(month, levels = c("April", "May", "June", "July"), ordered = TRUE),
    covid_results_date = factor(covid_results_date, levels = c("Feburary", "March", "April", "May"),
                                labels = c("February", "March", "April", "May"), ordered = TRUE),
    ethnicity = factor(ethnicity, levels = c("NA", "caucasian", "metis", "south asian", "caucasian;hispanic/latino"),
                       labels = c("NA", "Caucasian", "Metis", "South Asian", "Mixed")),
    across(c(needs, travel_work_school, fsa), factor)
  )

covid_long_clean <- covid %>%
  separate_rows(symptoms, sep = ";") %>%
  separate_rows(conditions, sep = ";") %>%
  separate_rows(financial_obligations_impact, sep = ";") %>%
  separate_rows(media_channels, sep = ";") %>%
  mutate(
    province = factor(case_when(
      str_sub(fsa, 1, 1) %in% c("K", "L", "M", "N", "P") ~ paste0("Ontario (",
                                                                  c("K" = "Eastern", "L" = "Central", "M" = "Toronto/GTA", "N" = "Southwestern", "P" = "Northern")[str_sub(fsa, 1, 1)], ")"),
      TRUE ~ "Other")),
    symptoms = ifelse(symptoms == "diarrhee", "diarrhea", symptoms),
    conditions = ifelse(conditions == "lhypertension", "highBloodPressure", conditions)
  )





# UI
ui <- page_navbar(
  title = "COVID-19 Survey Dashboard",
  theme = bs_theme(bootswatch = "litera"),
  
  nav_panel("Monthly Case Distribution Summary",
            layout_sidebar(
              sidebar = sidebar(
                width = 250,
                h4("Monthly Summary Controls"),
                h5("Cumulative Respondents Plot"),
                selectInput("cumulative_y_var", "Select Y-axis variable:",
                            choices = c("Total Records" = "total", "Cumulative Respondents" = "cumulative")),
                h5("Total Cases Plot"),
                selectInput("total_cases_y_var", "Select plot variable:",
                            choices = c("COVID Positive" = "covid_positive", "Vulnerable" = "vulnerable", "Tested" = "tested"))
              ),
              layout_columns(
                value_box(
                  title = span("Total Records"),
                  value = span(textOutput("stat_total")),
                  showcase = bsicons::bs_icon("database"),
                  theme = "primary"
                ),
                value_box(
                  title = span("COVID Positive"),
                  value = span(textOutput("stat_positive")),
                  showcase = bsicons::bs_icon("virus"),
                  theme = "danger"
                ),
                value_box(
                  title = span("Reported Symptoms"),
                  value = span(textOutput("stat_symptomatic")),
                  showcase = bsicons::bs_icon("exclamation-triangle-fill"),
                  theme = "info"
                )
              ),
              layout_columns(
                card(
                  card_body(
                    plotOutput("cumulative_plot", height = "50vh"),
                    padding = 1
                  ),
                  full_screen = TRUE
                ),
                card(
                  card_body(
                    plotOutput("total_cases_plot", height = "50vh"),
                    padding = 1
                  ),
                  full_screen = TRUE
                ),
                col_widths = c(6, 6)
              )
            )
  ),
  
  
  nav_panel("Demographic Profile of all Survey Participants",
            layout_sidebar(
              sidebar = sidebar(
                h4("Demographic Plot Controls"),
                
                selectInput("group_var", "Group by:",
                            choices = c("Region" = "province",
                                        "Ethnicity" = "ethnicity",
                                        "Age Group" = "age_1",
                                        "Sex" = "sex")),
                
                selectInput("filter_sex", "Filter by Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                
                selectInput("filter_age", "Filter by Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                
                selectInput("filter_ethnicity", "Filter by Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                
                selectInput("filter_region", "Filter by Region:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))
              ),
              layout_columns(
                card(plotOutput("demo_plot", height = "500px"), full_screen = TRUE,
                     card_header = "Demographic Profile"),
                col_widths = 12))
  ),
  
  nav_panel("Diagnostics: Testing Accuracy of Established Signs of COVID-19",
            layout_sidebar(
              sidebar = sidebar(
                h4("Test Accuracy of this Sign of COVID-19"),
                selectInput("symptom_var", "Choose Symptom Combination:",
                            choices = c(
                              "Fever/Chills/Shakes" = "fever_chills_shakes",
                              "Cough" = "cough",
                              "Shortness of Breath" = "shortness_of_breath",
                              "Fever + Cough" = "fcs_and_cough",
                              "Fever + SOB" = "fcs_sob",
                              "Cough + SOB" = "cough_sob",
                              "All Three" = "allsigns",
                              "Probable Case" = "probable"))
              ),
              
              layout_columns(
                card(plotOutput("symptom_positivity_plot", height = "400px"),
                     full_screen = TRUE,
                     card_header = "Symptom Predictive Accuracy"),
                col_widths = 12))
  ),
  
  nav_panel("Past History",
            layout_sidebar(
              sidebar = sidebar(
                h4("Filter Participants"),
                selectInput("filter_sex", "Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                selectInput("filter_age", "Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                selectInput("filter_ethnicity", "Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                selectInput("filter_province", "Province:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))
              ),
              
              tabsetPanel(
                tabPanel("Tobacco Use",
                         layout_columns(
                           card(plotOutput("tobacco_plot", height = "500px"),
                                full_screen = TRUE, card_header = "Tobacco Use by Population Group"),
                           col_widths = 12)
                ),
                
                tabPanel("Medical Conditions",
                         layout_columns(
                           card(plotOutput("medical_plot", height = "500px"),
                                full_screen = TRUE, card_header = "Medical History by Population Group"),
                           col_widths = 12)
                )
              )
            )
  ),
  nav_panel("Contact and Behaviour",
            layout_sidebar(
              sidebar = sidebar(
                h4("Filter Demographics"),
                selectInput("filter_sex", "Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                selectInput("filter_age", "Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                selectInput("filter_ethnicity", "Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                selectInput("filter_province", "Province:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))
              ),
              layout_columns(
                col_widths = 12,
                tabsetPanel(
                  tabPanel("Contact",
                           selectInput("contact_var", "Select Contact Variable:",
                                       choices = c("Contact with Illness" = "contact_with_illness",
                                                   "Contact in Household" = "contact_in_household")),
                           plotOutput("contact_plot", height = "400px")
                  ),
                  tabPanel("Behaviour",
                           selectInput("behaviour_var", "Select Behaviour Variable:",
                                       choices = c("Self-isolating" = "self_isolating",
                                                   "Travel Outside Canada" = "travel_outside_canada",
                                                   "Travel for Work/School" = "travel_work_school")),
                           plotOutput("behaviour_plot", height = "400px")
                  )
                )
              )
            )
  ),
  
  nav_panel("Reported Symptoms and Comorbidities",
            layout_sidebar(
              sidebar = sidebar(
                h4("Filter Participants"),
                selectInput("filter_sex", "Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                selectInput("filter_age", "Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                selectInput("filter_ethnicity", "Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                selectInput("filter_province", "Province:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))
              ),
              
              tabsetPanel(
                tabPanel("Symptoms", 
                         layout_columns(
                           card(plotOutput("top_symptoms_plot", height = "500px"),
                                full_screen = TRUE,
                                card_header = "Top 10 Reported Symptoms")
                         )
                ),
                tabPanel("Conditions", 
                         layout_columns(
                           card(plotOutput("top_conditions_plot", height = "500px"),
                                full_screen = TRUE,
                                card_header = "Top 10 Reported Comorbid Conditions"))
                )
              )
            )
  ),
  
  nav_panel("Impacts",
            layout_sidebar(
              sidebar = sidebar(
                h4("Filter Participants"),
                selectInput("filter_sex", "Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                selectInput("filter_age", "Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                selectInput("filter_ethnicity", "Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                selectInput("filter_province", "Province:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))
              ),
              
              h4("Behavioural Filters (for Financial & Mental Health Tabs)"),
              selectInput("self_isolating", "Adherence to isolation:",
                          choices = c("All", levels(droplevels(covid_long_clean$self_isolating)))),
              selectInput("travel_work_school", "Maintained Travel:",
                          choices = c("All", levels(droplevels(covid_long_clean$travel_work_school))))                            
              
            ),
            
            layout_columns(
              tabsetPanel(
                tabPanel("Top Needs",
                         card(
                           plotOutput("needs_plot", height = "450px"),
                           full_screen = TRUE,
                           card_header = "Top 10 Needs by Population"
                         )
                ),
                tabPanel("Financial Obligations",
                         card(
                           plotOutput("fin_impact_plot", height = "450px"),
                           full_screen = TRUE,
                           card_header = "Impacted Financial Obligations"
                         )
                ),
                tabPanel("Mental Health",
                         card(
                           plotOutput("mental_health_plot", height = "450px"),
                           full_screen = TRUE,
                           card_header = "Mental Health Impacts"
                         )
                ) 
              ),
              col_widths = 12
            )
  ),
  
  
  nav_panel("At-Risk Populations",
            layout_sidebar(
              sidebar = sidebar(
                h4("At-Risk Populations Controls"),
                h5("Risk Factor Heatmap Plot"),
                selectInput("risk_heatmap_var", "Risk Factor:",
                            choices = c("Financial Impact" = "financial_obligations_impact",
                                        "Mental Health Impact" = "mental_health_impact", "Needs" = "needs")),
                selectInput("risk_heatmap_demo", "Demographic variable:",
                            choices = c("Province" = "province", "Ethnicity" = "ethnicity", "Age" = "age_1", "Sex" = "sex")),
                h5("Detailed Risk Distribution Plot"),
                selectInput("risk_detailed_var", "Risk Factor:",
                            choices = c("Financial Impact" = "financial_obligations_impact",
                                        "Mental Health Impact" = "mental_health_impact", "Needs" = "needs")),
                selectInput("risk_detailed_demo", "Demographic variable:",
                            choices = c("Province" = "province", "Ethnicity" = "ethnicity", "Age" = "age_1", "Sex" = "sex")),
                h5("Overall Risk Distribution Plot"),
                selectInput("risk_summary_var", "Risk Factor:",
                            choices = c("Financial Impact" = "financial_obligations_impact",
                                        "Mental Health Impact" = "mental_health_impact", "Needs" = "needs"))
              ),
              h4("Risk factors across demographics"),
              layout_columns(
                card(plotOutput("risk_heatmap_plot", height = "400px"), full_screen = TRUE, card_header = "Risk Factor Heatmap"),
                col_widths = 12
              ),
              layout_columns(
                card(plotOutput("risk_detailed_plot", height = "300px"), full_screen = TRUE, card_header = "Detailed Risk Distribution"),
                card(plotOutput("risk_summary_plot", height = "300px"), full_screen = TRUE, card_header = "Overall Risk Distribution"),
                col_widths = c(6, 6)
              )
            )
  ),
  
  
  nav_panel("Media",
            layout_sidebar(
              sidebar = sidebar(
                h4("Filter Participants"),
                selectInput("filter_sex", "Sex:",
                            choices = c("All", levels(droplevels(covid_long_clean$sex)))),
                selectInput("filter_age", "Age Group:",
                            choices = c("All", levels(droplevels(covid_long_clean$age_1)))),
                selectInput("filter_ethnicity", "Ethnicity:",
                            choices = c("All", levels(droplevels(covid_long_clean$ethnicity)))),
                selectInput("filter_province", "Province:",
                            choices = c("All", levels(droplevels(covid_long_clean$province))))),
              
              layout_columns(
                card(
                  plotOutput("media_effectiveness_plot", height = "500px"),
                  full_screen = TRUE,
                  card_header = "Media Channel Preferences"
                ),
                col_widths = 12)
              
            )))

# Server
server <- function(input, output, session) {
  value_map <- list(covid_positive = "positively", vulnerable = "Yes", tested = "Yes")
  
  #DEMOGRAPHIC FILTER FOR EVERY SECTION
  filtered_data <- reactive({
    df <- covid_long_clean
    if (input$filter_sex != "All") {df <- df %>% filter(sex == input$filter_sex)}
    if (input$filter_age != "All") {df <- df %>% filter(age_1 == input$filter_age)}
    if (input$filter_ethnicity != "All") {df <- df %>% filter(ethnicity == input$filter_ethnicity)}
    if (input$filter_province != "All") {df <- df %>% filter(province == input$filter_province)}
    df
  })
  
  # Info stats
  output$stat_total <- renderText({
    format(nrow(covid_long_clean), big.mark = ",")
  })
  
  output$stat_positive <- renderText({
    format(sum(covid_long_clean$covid_positive == "positively", na.rm = TRUE), big.mark = ",")
  })
  
  output$stat_symptomatic <- renderText({
    format(sum(!is.na(covid_long_clean$symptoms) & covid_long_clean$symptoms != "" & covid_long_clean$symptoms != "NA", na.rm = TRUE), big.mark = ",")
  })
  
  #Monthly Case Distribution Summary Plots
  output$cumulative_plot <- renderPlot({
    plot_data <- covid_long_clean %>%
      count(month) %>%
      arrange(match(month, levels(covid_long_clean$month))) %>%
      mutate(cumulative = cumsum(n))
    
    y_var <- switch(input$cumulative_y_var,
                    "total" = "n",
                    "cumulative" = "cumulative")
    
    ggplot(plot_data, aes(x = month, y = .data[[y_var]], group = 1)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "red", size = 3) +
      labs(title = paste("Cumulative Respondents:", str_to_title(input$cumulative_y_var)),
           x = "Month",
           y = str_to_title(input$cumulative_y_var)) +
      theme_minimal()
  })
  
  output$total_cases_plot <- renderPlot({
    req(input$total_cases_y_var)
    filtered <- covid_long_clean %>%
      filter(!is.na(.data[[input$total_cases_y_var]]) & .data[[input$total_cases_y_var]] == value_map[[input$total_cases_y_var]]) %>%
      count(month) %>%
      # Ensure all months appear, even with 0 counts
      complete(month, fill = list(n = 0))
    
    ggplot(filtered, aes(x = month, n, y = n, fill = month)) +
      geom_col(color = "black", alpha = 0.8, show.legend = FALSE) +
      labs(title = paste("Total", str_to_title(gsub("_", " ", input$total_cases_y_var)), "Per Month"),
           x = "Month",
           y = "Count") +
      theme_minimal()
  })
  
  
  
  
  #Demographic Plot of All Survey Participants
  output$demo_plot <- renderPlot({
    req(input$group_var)
    
    df <- filtered_data()
    
    #Plotting
    plot_data <- df %>%
      count(group = .data[[input$group_var]]) %>%
      arrange(desc(n))
    validate(need(nrow(plot_data) > 0, "No data available for the selected filters."))
    
    
    ggplot(plot_data, aes(x = reorder(group, n), y = n, fill = group)) +
      geom_col(color = "black", alpha = 0.8) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Count",
        title = paste("Count by", tools::toTitleCase(gsub("_", " ", input$group_var)))
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #Diagnostics
  output$symptom_positivity_plot <- renderPlot({
    req(input$symptom_var)
    
    df <- covid_long_clean %>%
      filter(tested == "Yes", !is.na(covid_positive)) %>%
      mutate(symptom_present = .data[[input$symptom_var]],
             covid_result = recode(covid_positive,
                                   "positively" = "Positive",
                                   "negatively" = "Negative")) %>%
      mutate(symptom_status = if_else(symptom_present == "Yes", "Symptom Present", "No Symptom")) %>%
      count(symptom_status, covid_result) %>%
      group_by(symptom_status) %>%
      mutate(perc = round(100 * n / sum(n), 1))
    
    ggplot(df, aes(x = symptom_status, y = perc, fill = covid_result)) +
      geom_col(position = "dodge", alpha = 0.9) +
      labs(
        x = "Symptom Presence",
        y = "Percentage of Presence Diagnosed as Covid Positive (%)",
        fill = "COVID Test Result",
        title = paste("Percentage of Cases diagnosed as COVID Positive by:", input$symptom_var)
      ) +
      theme_minimal()
  })
  
  #PAST HISTORY
  
  # Tobacco Use Plot
  output$tobacco_plot <- renderPlot({
    df_all <- filtered_data() %>%
      mutate(group = "All")
    
    df_tested <- df_all %>% filter(!is.na(tested)) %>% mutate(group = "Tested")
    df_positive <- df_all %>% filter(!is.na(covid_positive)) %>% mutate(group = "COVID Positive")
    
    plot_data <- bind_rows(df_all, df_tested, df_positive) %>%
      filter(!is.na(tobacco_usage)) %>%
      count(group, tobacco_usage)
    
    validate(need(nrow(plot_data) > 0, "No data available for the selected filters."))
    
    ggplot(plot_data, aes(x = tobacco_usage, y = n, fill = group)) +
      geom_col(position = position_dodge(width = 0.8)) +
      labs(title = "Tobacco Use by Population Group",
           x = "Tobacco Use",
           y = "Count",
           fill = "Population") +
      theme_minimal()
  })
  
  # Medical Conditions Plot
  output$medical_plot <- renderPlot({
    df_all <- filtered_data() %>%
      mutate(group = "All")
    
    df_tested <- df_all %>% filter(!is.na(tested)) %>% mutate(group = "Tested")
    df_positive <- df_all %>% filter(!is.na(covid_positive)) %>% mutate(group = "COVID Positive")
    
    plot_data <- bind_rows(df_all, df_tested, df_positive) %>%
      filter(!is.na(any_medical_conditions)) %>%
      count(group, any_medical_conditions)
    
    validate(need(nrow(plot_data) > 0, "No data available for the selected filters."))
    
    
    ggplot(plot_data, aes(x = any_medical_conditions, y = n, fill = group)) +
      geom_col(position = position_dodge(width = 0.8)) +
      labs(title = "Medical History by Population Group",
           x = "Any Medical Conditions",
           y = "Count",
           fill = "Population") +
      theme_minimal()
  })
  
  
  #CONTACT AND BEHAVIOUR 
  
  # Plotting logic for Population Columns
  plot_cb_variable <- function(df, var) {
    pop_groups <- list(
      "Across all participants" = df,
      "Tested" = df %>% filter(tested == "Yes"),
      "COVID Positive" = df %>% filter(covid_positive == "positively"),
      "COVID Negative" = df %>% filter(!is.na(tested) & tested == "negatively" & is.na(covid_positive)),
      "Vulnerable" = df %>% filter(vulnerable == "Yes"),
      "Vulnerable + Positive" = df %>% filter(vuln_and_pos == "Yes") )
    
    group_list <- list()
    for (group_name in names(pop_groups)) {
      d <- pop_groups[[group_name]]
      if (!var %in% names(d)) next
      
      group_data <- d %>%
        count(!!sym(var), name = "count") %>%
        mutate(group = group_name)
      
      group_list[[group_name]] <- group_data
    }
    
    plot_data <- bind_rows(group_list)
    
    validate(need(nrow(plot_data) > 0, "No data available for the selected filters."))
    
    plot_data[[var]] <- as.character(plot_data[[var]])
    plot_data[[var]][is.na(plot_data[[var]])] <- "Missing"
    plot_data[[var]] <- factor(plot_data[[var]])
    
    ggplot(plot_data, aes_string(x = var, y = "count", fill = "group")) +
      geom_col(position = "dodge", alpha = 0.9) +
      labs(x = NULL, y = "Count", fill = "Population Group") +
      theme_minimal()
  }
  
  # Contact plot
  output$contact_plot <- renderPlot({
    req(input$contact_var)
    df <- filtered_data()
    
    plot_cb_variable(df, input$contact_var)
  })
  
  # Behaviour plot
  output$behaviour_plot <- renderPlot({
    req(input$behaviour_var)
    df <- filtered_data()
    plot_cb_variable(df, input$behaviour_var)
  })
  
  
  #REPORTED SYMPTOMS AND COMORBIDITIES
  
  #SYMPTOMS plot
  output$top_symptoms_plot <- renderPlot({
    df <- filtered_data()
    
    df <- df %>%
      filter(!is.na(symptoms)) %>%
      mutate(
        population_group = case_when(
          vuln_and_pos == "Yes" ~ "Vulnerable + Positive",
          covid_positive == "positively" ~ "Positive",
          TRUE ~ NA_character_)) %>%
      
      filter(!is.na(population_group)) %>%
      count(symptoms, population_group, sort = TRUE)
    
    top_symptoms <- df %>%
      group_by(symptoms) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      top_n(10, total) %>%
      pull(symptoms)
    
    df_filtered <- df %>% filter(symptoms %in% top_symptoms)
    
    validate(need(nrow(df_filtered) > 0, "No data available for the selected filters."))
    
    ggplot(df_filtered, aes(x = reorder(symptoms, n), y = n, fill = population_group)) +
      geom_col(position = "stack") +
      coord_flip() +
      labs(
        x = "Symptom",
        y = "Count",
        fill = "Population Group",
        title = "Top 10 Reported Symptoms by COVID Status"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  #CONDITIONS plot
  output$top_conditions_plot <- renderPlot({
    df <- filtered_data()
    
    df <- df %>%
      filter(!is.na(conditions)) %>%
      mutate(
        population_group = case_when(
          vuln_and_pos == "Yes" ~ "Vulnerable + Positive",
          covid_positive == "positively" ~ "Positive",
          TRUE ~ NA_character_)) %>%
      
      filter(!is.na(population_group)) %>%
      count(conditions, population_group, sort = TRUE)
    
    top_conditions <- df %>%
      group_by(conditions) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      top_n(10, total) %>%
      pull(conditions)
    
    df_filtered <- df %>% filter(conditions %in% top_conditions)
    
    validate(need(nrow(df_filtered) > 0, "No data available for the selected filters."))
    
    ggplot(df_filtered, aes(x = reorder(conditions, n), y = n, fill = population_group)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Condition",
        y = "Count",
        fill = "Population Group",
        title = "Top Reported Conditions by COVID Status"
      ) +
      theme_minimal()
  })
  
  
  #IMPACTS
  
  # Utility function for filtering by demographics & behaviour
  
  filtered_impact_data <- reactive({
    df <- covid_long_clean
    
    # Demographics
    if (!is.null(input$filter_sex) && input$filter_sex != "All") {
      df <- df %>% filter(sex == input$filter_sex)
    }
    if (!is.null(input$filter_age) && input$filter_age != "All") {
      df <- df %>% filter(age_1 == input$filter_age)
    }
    if (!is.null(input$filter_ethnicity) && input$filter_ethnicity != "All") {
      df <- df %>% filter(ethnicity == input$filter_ethnicity)
    }
    if (!is.null(input$filter_province) && input$filter_province != "All") {
      df <- df %>% filter(province == input$filter_province)
    }
    
    # Behavioral filters — only for Mental Health / Financial tabs
    if (!is.null(input$self_isolating) && input$self_isolating != "All") {
      df <- df %>% filter(self_isolating == input$self_isolating)
    }
    
    if (!is.null(input$travel_work_school) && input$travel_work_school != "All") {
      df <- df %>% filter(travel_work_school == input$travel_work_school)
    }
    
    return(df)
  })
  
  
  #Needs Plot
  output$needs_plot <- renderPlot({
    df <- filtered_impact_data()
    
    df_long <- df %>%
      filter(!is.na(needs)) %>%
      count(needs, covid_group = case_when(
        vuln_and_pos == "Yes" ~ "Vulnerable + Positive",
        covid_positive == "positively" ~ "Positive",
        TRUE ~ "All"
      ))
    
    top_needs <- df_long %>%
      filter(covid_group == "All") %>%
      arrange(desc(n)) %>%
      slice_max(n = 10, order_by = n) %>%
      pull(needs)
    
    df_filtered <- df_long %>%
      filter(needs %in% top_needs)
    
    validate(need(nrow(df_filtered) > 0, "No data available for the selected filters."))
    
    ggplot(df_filtered, aes(x = reorder(needs, n), y = n, fill = covid_group)) +
      geom_col(position = "stack", alpha = 0.85) +
      coord_flip() +
      labs(
        x = "Need Reported",
        y = "Count",
        title = "Top 10 Reported Needs by Population Type",
        fill = "Population"
      ) +
      theme_minimal()
  })
  
  #Financial Obligations Impact Plot
  output$fin_impact_plot <- renderPlot({
    df <- filtered_impact_data()
    
    df_long <- df %>%
      filter(!is.na(financial_obligations_impact)) %>%
      count(financial_obligations_impact, covid_group = case_when(
        vuln_and_pos == "Yes" ~ "Vulnerable + Positive",
        covid_positive == "positively" ~ "Positive",
        TRUE ~ "All"
      ))
    
    top_fin <- df_long %>%
      filter(covid_group == "All") %>%
      arrange(desc(n)) %>%
      slice_max(n = 10, order_by = n) %>%
      pull(financial_obligations_impact)
    
    df_filtered <- df_long %>%
      filter(financial_obligations_impact %in% top_fin)
    
    validate(need(nrow(df_filtered) > 0, "No data available for the selected filters."))
    
    ggplot(df_filtered, aes(x = reorder(financial_obligations_impact, n), y = n, fill = covid_group)) +
      geom_col(position = "stack", alpha = 0.85) +
      coord_flip() +
      labs(
        x = "Financial Obligation Impacted",
        y = "Count",
        title = "Top 10 Financial Impacts by Population Type",
        fill = "Population"
      ) +
      theme_minimal()
  })
  
  #Mental Health Impact Plot
  output$mental_health_plot <- renderPlot({
    df <- filtered_impact_data()
    
    df_long <- df %>%
      filter(!is.na(mental_health_impact)) %>%
      count(mental_health_impact, covid_group = case_when(
        vuln_and_pos == "Yes" ~ "Vulnerable + Positive",
        covid_positive == "positively" ~ "Positive",
        TRUE ~ "All"
      ))
    
    top_mh <- df_long %>%
      filter(covid_group == "All") %>%
      arrange(desc(n)) %>%
      slice_max(n = 10, order_by = n) %>%
      pull(mental_health_impact)
    
    df_filtered <- df_long %>%
      filter(mental_health_impact %in% top_mh)
    
    validate(need(nrow(df_filtered) > 0, "No data available for the selected filters."))
    
    ggplot(df_filtered, aes(x = reorder(mental_health_impact, n), y = n, fill = covid_group)) +
      geom_col(position = "stack", alpha = 0.85) +
      coord_flip() +
      labs(
        x = "Mental Health Impact",
        y = "Count",
        title = "Top 10 Mental Health Impacts by Population Type",
        fill = "Population"
      ) +
      theme_minimal()
  })
  
  
  # At-Risk Populations Plots
  output$risk_heatmap_plot <- renderPlot({
    req(input$risk_heatmap_var, input$risk_heatmap_demo)
    plot_data <- covid_long_clean %>%
      filter(!is.na(.data[[input$risk_heatmap_var]]), !is.na(.data[[input$risk_heatmap_demo]]),
             .data[[input$risk_heatmap_var]] != "", .data[[input$risk_heatmap_var]] != "NA") %>%
      count(.data[[input$risk_heatmap_demo]], .data[[input$risk_heatmap_var]])
    
    p <- ggplot(plot_data, aes(x = .data[[input$risk_heatmap_demo]], y = .data[[input$risk_heatmap_var]], fill = n)) +
      geom_tile(color = "white") +
      geom_text(aes(label = n), color = "white", size = 3) +
      scale_fill_gradient(name = "Count") +
      labs(title = paste("Risk Factor Heatmap:", str_to_title(gsub("_", " ", input$risk_heatmap_var))),
           x = str_to_title(gsub("_", " ", input$risk_heatmap_demo)),
           y = str_to_title(gsub("_", " ", input$risk_heatmap_var))) +
      theme_minimal()
    
    if (input$risk_heatmap_var == "financial_obligations_impact") {
      p <- p + coord_flip()
    }
    
    p
  })
  
  output$risk_detailed_plot <- renderPlot({
    req(input$risk_detailed_var, input$risk_detailed_demo)
    plot_data <- covid_long_clean %>%
      filter(!is.na(.data[[input$risk_detailed_var]]), !is.na(.data[[input$risk_detailed_demo]]),
             .data[[input$risk_detailed_var]] != "", .data[[input$risk_detailed_var]] != "NA") %>%
      count(.data[[input$risk_detailed_demo]], .data[[input$risk_detailed_var]])
    
    p <- ggplot(plot_data, aes(x = reorder(.data[[input$risk_detailed_demo]], n), y = n, fill = .data[[input$risk_detailed_var]])) +
      geom_col(position = "stack") +
      labs(
        title = paste("Detailed Risk Distribution:", str_to_title(gsub("_", " ", input$risk_detailed_var))),
        x = str_to_title(gsub("_", " ", input$risk_detailed_demo)),
        y = "Count",
        fill = str_to_title(gsub("_", " ", input$risk_detailed_var))) +
      theme_minimal()
    
    if (input$risk_detailed_var == "financial_obligations_impact") {
      p <- p + coord_flip()
    }
    
    p
  })
  
  output$risk_summary_plot <- renderPlot({
    req(input$risk_summary_var)
    plot_data <- covid_long_clean %>%
      filter(!is.na(.data[[input$risk_summary_var]]), .data[[input$risk_summary_var]] != "", .data[[input$risk_summary_var]] != "NA") %>%
      count(.data[[input$risk_summary_var]])
    
    p <- ggplot(plot_data, aes(x = reorder(.data[[input$risk_summary_var]], n), y = n, fill = .data[[input$risk_summary_var]])) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Overall Risk Distribution:", str_to_title(gsub("_", " ", input$risk_summary_var))),
        x = str_to_title(gsub("_", " ", input$risk_summary_var)),
        y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    p
  })
  
  
  # Media Effectiveness Plot
  output$media_effectiveness_plot <- renderPlot({
    df <- filtered_data()
    
    plot_data <- df %>%
      filter(!is.na(media_channels), media_channels != "", media_channels != "NA") %>%
      count(media_channels, sort = TRUE) %>%
      slice_max(n, n = 10)
    
    validate(need(nrow(plot_data) > 0, "No data available for the selected filters."))
    
    ggplot(plot_data, aes(x = reorder(media_channels, n), y = n, fill = media_channels)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top 10 Most Effective Media Channels",
        x = "Media Channel",
        y = "Number of Participants"
      ) +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
