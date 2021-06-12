# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

#read data
mobility <- read_csv("Mobility_Report.csv")
mobility$Date <- as.Date(mobility$Date)
mobility$Province <- as.factor(mobility$Province)

#ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("COVID-19 Mobility Data"),
      selectInput(inputId = "dv", label = "Category",
                  choices = c("Retail_Recreation", "Grocery_Pharmarcy", "Parks", "Transit_Stations", "Workplaces", "Residential"),
                  selected = "Grocery_Pharmarcy"),
      selectInput(inputId = "provinces", "Province(s)",
                  choices = levels(mobility$Province),
                  multiple = TRUE,
                  selected = c("Brazil","Australia","Argentina")),
      dateRangeInput(inputId = "date", "Date range",
                     start = min(mobility$Date),
                     end   = max(mobility$Date)),
      downloadButton(outputId = "Global_Mobility_Report", label = "Global_Mobility_Report_dataset"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Positive and negative percentages represent an increase or decrease from the baseline period."),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

#server
server <- function(input, output) {
  filtered_data <- reactive({
    subset(mobility,
           Province %in% input$provinces &
             Date >= input$date[1] & Date <= input$date[2])})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="Date", y=input$dv, color="Province")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("Percentage change from baseline")
      
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
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
