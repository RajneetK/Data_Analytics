# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(dplyr)
library(glue)
library(plotly)
library(shiny)

provider <- read_csv("Provider.csv")

# ui ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(includeHTML(("https://catalog.data.gov/dataset/provider-relief-fund-covid-19-high-impact-payments"))),
  
  titlePanel(" Relief Fund COVID-19 "),
  
  helpText("Use this Shiny app to explore the distribution of relief funds to hospitals and other healthcare providers on the front lines of the coronavirus response."),
  
  br(),
  
  selectInput("Name", "City",
              choices = c(
                "First round payment" = "First_payment",
                "Second round payment" = "Second_payment",
                "States" = "State"
              )
  ),
  
  actionButton("update_plot", "Update Plot"),
  
  br(),
  
  plotOutput("map", width = "90%")
)

# server -----------------------------------------------------------------------

server <- function(input, output) {
  high <- reactive({
    switch(input$Name,
           "First Round Payment" = "yellow",
           "Second Round Payment" = "yellow",
           "States" = "whitesmoke"
    )
  })
  
  low <- reactive({
    switch(input$Name,
           "First Round Payment" = "darkorange1",
           "Second Round Payment" = "steelblue",
           "States" = "steelblue"
    )
  })
  
  updated_plot <- eventReactive(input$update_plot, {
    ggplot(provider, aes(Name, State, City = City)) +
      geom_polygon(aes_string(fill = input$Name), color = "white") +
      scale_fill_gradient(input$Name, low = low(), high = high()) +
      theme_minimal() +
      labs(x = "", y = "")
  },
  ignoreNULL = FALSE)
  
  output$map <- renderPlot({
    updated_plot()
  })
}

# shiny app --------------------------------------------------------------------

shinyApp(ui=ui, server=server)
