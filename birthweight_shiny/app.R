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

# Data Import
ny_map = read_csv("birthweight_shiny/NY_map.csv")

# UI setting
ui <- fluidPage(
  titlePanel("New York Counties: PM2.5 and Health Outcomes"),
  sidebarLayout(
    sidebarPanel(
      # Add any input controls here
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Correlations", plotOutput("correlationPlot")),
        tabPanel("Comparison", dataTableOutput("comparisonTable")),
        tabPanel("Data Table", DT::dataTableOutput("dataTable"))
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
    selected_health_outcome <- input$healthOutcome  # assuming you have a selectInput for health outcomes
    ggplot(ny_map, aes_string(x = "annual_pm2.5", y = selected_health_outcome)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Annual PM2.5", y = selected_health_outcome) +
      theme_minimal()
  })
  
  # Comparison table
  output$comparisonTable <- renderDataTable({
    selected_counties <- input$selectedCounties  # assuming you have a selectInput for counties
    ny_map %>% 
      filter(county %in% selected_counties)
  })
  
  # Data table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(ny_map)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
