library(shiny)
library(dplyr)
library(plotly)
library(gapminder)

ui <- fluidPage(
  
  titlePanel("Simulations of a random walk process"),
  h5(HTML("Based on Heiss, F. <i>Using R for Introductory Econometrics</i>, pp. 200-202."), tags$a(href="https://www.urfie.net/", "https://www.urfie.net/")),
  h5("R Shiny by", tags$a(href = "https://www.alessandrobramucci.com/", tags$i("Alessandro Bramucci",))),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("n_periods", "Number of periods", 1, 100, 50, 1),
      sliderInput("n_scenarios", "Number of scenarios", 1, 100, 30, 1),
      sliderInput("initial_value", "Initial value", -5, 5, 0, 1),
      sliderInput("drift", "Drift", -5, 5, 0, 1),
      sliderInput("deviation", "Standard deviation", 0, 5, 1, 1)
      
    ),
    
    mainPanel(
      plotlyOutput("plot")
  )
 )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    set.seed(348546)
  
    p <- plot_ly() %>% 
      add_trace(x = c(0, input$n_periods),
                y = c(input$initial_value, input$initial_value + input$n_periods * input$drift),
                type = "scatter",
                mode = "lines",
                hoverinfo = "none",
                line = list(color = "black")) %>% 
      config(displayModeBar = F)
    
    for (r in 1:input$n_scenarios) {
      
      e <- rnorm(input$n_periods, input$drift, input$deviation)
      
      y <- input$initial_value + cumsum(e)
      
      p <- p %>%
        add_trace(x = 1:input$n_periods,
                  y = y,
                  type = "scatter",
                  mode = "lines",
                  name = paste0("Random walk ", r),
                  showlegend = F,
                  line = list(color = "lightgrey"))
      
    }
    
    p
    
  })
  
}

shinyApp(ui = ui, server = server)