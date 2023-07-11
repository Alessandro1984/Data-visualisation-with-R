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
  
  data_FS <- eventReactive(input$calculate, {
    
    data %>% 
      filter(Country == input$country)
    
  }, ignoreNULL = FALSE)
  
  output$plot <- renderPlotly({
  
    p <- plot_ly() %>%
      add_trace(x = data_FS()$Year,
                y = round(data_FS()$Government, digits = 2),
                type = "bar",
                name = "Government sector",
                marker = list(color = "green"),
                hoverlabel = list(namelength = -1)) %>%
      add_trace(x = data_FS()$Year,
                y = -round(data_FS()$Foreign, digits = 2),
                type = "bar",
                name = "Foreign sector",
                hoverlabel = list(namelength = -1),
                marker = list(color = "red")) %>%
      add_trace(x = data_FS()$Year,
                y = round(data_FS()$Households, digits = 2),
                type = "bar",
                name = "Household sector",
                hoverlabel = list(namelength = -1),
                marker = list(color = "blue")) %>%
      add_trace(x = data_FS()$Year,
                y = round(data_FS()$Corporations, digits = 2),
                type = "bar",
                name = "Corporate sector",
                hoverlabel = list(namelength = -1),
                marker = list(color = "darkblue")) %>%
      layout(hovermode = "x",
             xaxis = list(title = "",
                          dtick = 1,
                          tickangle = 45,
                          font = list(size = 10)),
             yaxis = list(title = "Percent of GDP"),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.15)) %>% 
      layout(barmode = "relative") %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
}

shinyApp(ui = ui, server = server)