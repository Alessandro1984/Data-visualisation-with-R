library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(gapminder)

data <- read_excel("data_FS.xlsx")

countries <- unique(data$Country)

ui <- fluidPage(
  
  titlePanel("Sectoral financial balances"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("country", 
                  "Select a country", 
                  choices = countries,
                  selected = "Germany"),
      
      actionButton("calculate", "Show")
      
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  # Use eventReactive
  
  output$plot <- renderPlotly({
    
  # Plotly here
    
  })
  
}

shinyApp(ui = ui, server = server)