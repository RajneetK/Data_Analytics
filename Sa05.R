# Load packages
library(shiny)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(DT)
library(lubridate)

# Load data
hotels<- read.csv("hotel_bookings.csv")
hotels <- hotels[1:50000,]

#Changing date columns to Date class, year to a factor
hotels$reservation_status_date <- as.Date(hotels$reservation_status_date,'%Y-%m-%d')
hotels$arrival_date_year <- as.factor(hotels$arrival_date_year) 

#create subset of data by which reservations were canceled
cancel <- hotels %>% filter(is_canceled==1)

# select four columns from the data as a subset for two chi square tests
guest <- hotels %>% 
  select(Canceled =is_canceled,History = previous_cancellations, 
         Requested = reserved_room_type, Assigned = assigned_room_type)

# creating three new columns to use as parameters for chi square tests
guest <- guest %>% 
  mutate(Canceled = ifelse(Canceled == 1,'Yes','No'),
         RequestedRoom = ifelse(Requested == Assigned,'Same','Not Same'),
         History = ifelse(History >=1, 'Yes','No'))

# chi square results for cancellation history and canceling upcoming stay
cancel_results <- chisq.test(x = guest$History, y = guest$Canceled)

# chi square results for room assignment and canceling upcoming stay
room_results <- chisq.test(x = guest$RequestedRoom, y = guest$Canceled)

#Hotel shiny dashboard

ui <- fluidPage(
  
  tags$div(class = "submit",
           tags$a(href = "https://www.kaggle.com/jessemostipak/hotel-booking-demand",
                  "Dataset link",
                  target="click")
  ),
  titlePanel(" Hotel booking Reports " ),
  
  helpText("Use this Shiny app to explore these Hotel booking Reports."),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      h2("Mobility Data"),
      
      selectInput(inputId = "dv", label = "Category",
                  choices = c("lead_time"),
                  selected = "Direct"),
      selectInput(inputId = "hotel", "hotels(s)",
                  choices = levels(hotels$hotel),
                  multiple = TRUE,
                  selected = c("City hotel","Resort hotel")),
      dateRangeInput(inputId = "reservation_status_date", "Date range",
                     start = min(hotels$reservation_status_date),
                     end   = max(hotels$reservation_status_date)),
      downloadButton(outputId = "Hotel_Report", label = "Hotel_booking_Report_dataset"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Hotel booking detail"),
      br(), br(), br(),
      
      
      
    )
  )
)

# Hotel shiny app server
server <- function(input, output,session) {
  
  filtered_data <- reactive({
    subset(hotels,
          hotel%in% input$hotel &
             Date >= input$date[1] & Date <= input$date[2])})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="Date", y=input$dv, color="hotel")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("Percentage change from baseline")
      
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

