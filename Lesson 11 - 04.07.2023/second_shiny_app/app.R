library(shiny)
library(dplyr)
library(plotly)
library(gapminder)

# Objects that do not need to be reactive
df_plot <- gapminder %>% 
  group_by(year) %>% 
  summarise(min_value = min(lifeExp), 
            max_value = max(lifeExp),
            med_value = median(lifeExp))

countries <- unique(gapminder$country)

# Start of user interface
ui <- fluidPage(
  
  # Title
  titlePanel("Life expectancy at birth"),
  
  # Select a country
  sidebarLayout(
    sidebarPanel(
      selectInput("country", 
                  "Select a country", 
                  choices = countries,
                  selected = "China"),
    ),
    
    # Plot
    mainPanel(
      plotlyOutput("plot"), #, width = "500px", height = "400px"
      # Just a note
      h6("Source: Gapminder.", align = "right", style = "color: red;")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Object reacting to users selection
  df_plot_final <- reactive({
    gapminder %>%
      filter(country == input$country) %>% 
      select(year, lifeExp) %>% 
      rename(lifeExp_country = lifeExp) %>% 
      inner_join(df_plot, by = "year")
  })
  
  # Plot
  output$plot <- renderPlotly({
    
    p <- plot_ly() %>% 
      add_trace(x = df_plot_final()$year,
                y = df_plot_final()$max_value,
                type = "scatter",
                mode = "lines",
                line = list(color = "blue"),
                name = "Maximum") %>% 
      add_trace(x = df_plot_final()$year,
                y = df_plot_final()$min_value,
                type = "scatter",
                mode = "lines",
                line = list(color = "red"),
                name = "Minimum") %>% 
      add_trace(x = df_plot_final()$year,
                y = df_plot_final()$med_value,
                type = "scatter",
                mode = "lines",
                line = list(color = "orange"),
                name = "Median") %>% 
      add_trace(x = df_plot_final()$year,
                y = df_plot_final()$lifeExp_country,
                type = "scatter",
                mode = "lines",
                line = list(color = "black"),
                name = input$country) %>% 
      layout(xaxis = list(title = "",
                          dtick = 5,
                          tickangle = -45),
             yaxis = list(title = "Life expectancy at birth"),
             hovermode = "x",
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.15)) %>% 
      config(displayModeBar = F)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)