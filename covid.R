# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)
library(DT)


mobility <- read.csv("Global_Mobility_Report.csv")

#ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("COVID-19 Mobility Data"),
      selectInput(inputId = "dv", label = "Category",
                  choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                  selected = "Grocery_Pharmarcy"),
      selectInput(inputId = "country_region",
                  label = "Select country_region",
                  choices = c("Utrecht", "Friesland", "Zeeland")),
      dateRangeInput(inputId = "date", "date range",
                     start = min(mobility$date),
                     end   = max(mobility$date)),
      downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot")
    )
  )
)

#server
server <- function(input, output) {
    output$plot <- renderPlotly({
    ggplotly({
      p <- mobility %>%
        filter(country_region == input$country_region) %>%
        filter(between(date, input$date[1], input$date[2])) %>%
        count(date, country_region) %>%
        
        ggplot(aes_string(x = date, y = input$dv)) +
        geom_line(color = "gray") +
        geom_point(alpha=0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +theme(legend.position = "none") +
        labs(
          title = glue("Daily sentiment score, {input$origin} COVID-19 briefings"),
          x = "Date", y = "% change from baseline") +
        theme_minimal() +
        theme(legend.position = "none")
      ggplotly(p, tooltip = c("x", "y"))
         
      
      p
    })
  })
  
  
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

#shinyapp

shinyApp(ui = ui, server = server)

