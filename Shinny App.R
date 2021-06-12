# load libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

#read data
covid_speeches_words <- read_rds("covid-speeches-words.rds")

# ui
ui <- fluidPage(
  titlePanel("Scotland and UK COVID-19 Speeches"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "origin",
        label = "Select origin",
        choices = c("Scotland", "UK")
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Select date range",
        start = min(covid_speeches_words$date),
        end = max(covid_speeches_words$date)
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "sentiment_plot")
    )
  )
)

#server
server <- function(input, output) {
  output$sentiment_plot <- renderPlotly({
    p <- covid_speeches_words %>%
      filter(origin == input$origin) %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      anti_join(stop_words) %>%
      filter(word != "positive") %>%
      inner_join(get_sentiments("bing"), by = "word") 