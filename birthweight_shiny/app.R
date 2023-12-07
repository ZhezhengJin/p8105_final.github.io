#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(shinythemes)
library(readxl)
library(plotly)

# Data Import
ny_map = read_csv("ny_map.csv")

# UI setting
ui <- fluidPage(
  theme = shinytheme("superhero"),  
  
  tags$head(
    tags$style(HTML("
          /* Custom CSS for DataTables */
          .dataTables_wrapper {
              color: #FFFFFF !important;;  
          }

          /* Custom CSS for table headers */
          table.dataTable thead th, table.dataTable thead td {
              color: #FFFFFF !important;; 
          }

          /* Custom CSS for table body */
          table.dataTable tbody th, table.dataTable tbody td {
              color: #FFFFFF !important;;  
          }

          /* Custom CSS for table footer */
          table.dataTable tfoot th, table.dataTable tfoot td {
              color: #FFFFFF !important;;  
          }
          
          /* Custom CSS for search input */
          .dataTables_filter input {
              color: #000000 !important;  /* Set your desired color */
              background-color: #FFFFFF !important;  /* Set your desired background color */
          }
          
          /* Custom CSS for pagination info */
          .dataTables_info {
              color: #FFFFFF !important;  /* Set your desired color */
          }
          
          /* Custom CSS for pagination buttons */
          .dataTables_wrapper .dataTables_paginate .paginate_button {
              color: #000000 !important; /* Text color for pagination buttons */
          }

          /* Custom CSS for active pagination button */
          .dataTables_wrapper .dataTables_paginate .paginate_button.current, 
          .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
              color: #000000 !important; /* Text color for the active pagination button */
              background-color: #f0ad4e !important; /* Background color for the active pagination button */
          }
          
          /* Custom CSS for the 'Show entries' dropdown */
          .dataTables_length label select {
              color: #000000 !important; /* Text color for dropdown */
          }
      "))
  ),
  
  titlePanel("New York Counties: PM2.5 and Health Outcomes Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("healthOutcome", 
                  "Select Health Outcome for Correlation:",
                  choices = c("percent_lowbirthweight", "premature_percentage", 
                              "cancer_mortality_per_100k", "asthma_hosp_rate_per_10k", 
                              "cardio_hosp_rate_per_10k")),
      
      selectInput("selectedCounties", 
                  "Select Counties for Comparison:",
                  choices = ny_map$county,  
                  multiple = TRUE),
  ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map"),
                 tags$p("This map visualizes PM2.5 air quality levels across New York counties, highlighting areas of concern and allowing for a detailed geographical exploration of environmental health data.")),
        
        tabPanel("Correlation", 
                 plotOutput("correlationPlot"),
                 tags$p("This plot reveals the relationship between PM2.5 levels and health outcomes, illustrating potential links between air quality and public health across New York counties. Each data point represents a unique county, providing a comparative analysis of how air quality correlates with health indicators such as low birthweight, cancer mortality, and more.")),
        
        tabPanel("Comparison", 
                 DT::dataTableOutput("comparisonTable"),
                 tags$p("This interactive table compares selected counties, showcasing average PM2.5 levels and health statistics to highlight environmental and public health disparities across New York.")),
        
        tabPanel("PM2.5 Bar Chart", 
                 plotlyOutput("barChart"),
                 tags$p("This interactive bar chart displays the annual average PM2.5 concentration across the top 20 counties in New York, where PM2.5 are tiny atmospheric particles less than 2.5 micrometers in diameter. The chart ranks counties in descending order of PM2.5 levels, emphasizing those with the highest concentrations. Hovering over each bar reveals the exact PM2.5 value for that county."))
      )
    )
  )
)

# Server setting
server <- function(input, output) {
  # Map with markers
  output$map <- renderLeaflet({
    leaflet(ny_map) %>% 
      addTiles() %>% 
      addMarkers(~lng, ~lat, popup = ~paste(county, "<br>",
                                            "PM2.5: ", annual_pm2.5, "<br>",
                                            "Low Birthweight: ", percent_lowbirthweight, "%", "<br>",
                                            "Premature: ", premature_percentage, "%", "<br>",
                                            "Cancer Mortality: ", cancer_mortality_per_100k, "<br>",
                                            "Cardiovascular Hospitalization: ", cardio_hosp_rate_per_10k, "<br>",
                                            "Asthma Hospitalization: ", asthma_hosp_rate_per_10k))
  })
  
  # Correlation plots
  output$correlationPlot <- renderPlot({
    req(input$healthOutcome)
    
    # A named vector that maps column names to more descriptive labels
    health_outcome_labels <- c(
      percent_lowbirthweight = "Percentage of Low Birthweight",
      premature_percentage = "Percentage of Premature Births",
      cancer_mortality_per_100k = "Cancer Mortality per 100k",
      asthma_hosp_rate_per_10k = "Asthma Hospitalization Rate per 10k",
      cardio_hosp_rate_per_10k = "Cardiovascular Hospitalization Rate per 10k"
    )
    
    # Use the selected health outcome to get the specific label
    specific_label <- health_outcome_labels[input$healthOutcome]
    
    # Generate the plot with the specific y-axis label
    ggplot(ny_map, aes_string(x = "annual_pm2.5", y = input$healthOutcome)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Annual PM2.5 Levels (µg/m³)", y = specific_label, title = paste("Correlation between PM2.5 Levels and", specific_label)) +
      theme_minimal()
  })
  
  # Comparison table
  output$comparisonTable <- renderDataTable({
    req(input$selectedCounties)
    
    # Filter the dataset for selected counties
    selected_data <- ny_map %>%
      filter(county %in% input$selectedCounties) %>%
      # For example, calculate mean values for each health outcome
      group_by(county) %>%
      summarise(
        mean_annual_pm2_5 = mean(annual_pm2.5, na.rm = TRUE),
        mean_percent_lowbirthweight = mean(percent_lowbirthweight, na.rm = TRUE),
        mean_premature_percentage = mean(premature_percentage, na.rm = TRUE),
        mean_cancer_mortality_per_100k = mean(cancer_mortality_per_100k, na.rm = TRUE),
        mean_asthma_hosp_rate_per_10k = mean(asthma_hosp_rate_per_10k, na.rm = TRUE),
        mean_cardio_hosp_rate_per_10k = mean(cardio_hosp_rate_per_10k, na.rm = TRUE)
      ) %>%
      ungroup()  # Ungroup for the DataTable
    
    # Render the DataTable with the selected and aggregated data
    DT::datatable(selected_data, options = list(pageLength = 5))
  })
  
  # Bar chart
  output$barChart <- renderPlotly({
    # Prepare the data: sort the dataframe by 'annual_pm2.5' and get the top 20
    sorted_data <- ny_map %>%
      arrange(desc(annual_pm2.5)) %>%
      head(20) %>%
      mutate(county = factor(county, levels = rev(unique(county))))
    
    # Create an interactive bar chart with plotly
    fig <- plot_ly(
      data = sorted_data,
      x = ~county,
      y = ~annual_pm2.5,
      type = 'bar',
      color = ~county,
      text = ~paste('PM2.5: ', annual_pm2.5),  # Hover text
      hoverinfo = 'text'
    ) %>%
      layout(
        title = 'Top 20 New York Counties by Annual PM2.5 Levels',
        xaxis = list(title = 'Counties'),
        yaxis = list(title = 'Annual PM2.5')
      )
    
    fig
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
