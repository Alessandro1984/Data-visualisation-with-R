library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(gapminder)

df_continent <- gapminder %>% 
  group_by(continent, year) %>% 
  summarise(across(c("lifeExp", "pop", "gdpPercap"), ~ mean(.x))) %>% 
  pivot_longer(!c(continent, year), names_to = "variable", values_to = "value") %>% 
  mutate(
    variable = case_when(
      variable == "lifeExp" ~ "Life expectancy at birth",
      variable == "pop" ~ "Population",
      variable == "gdpPercap" ~ "GDP per capita (in dollar)"
    )
  )

continents <- unique(df_continent$continent)

variables <- unique(df_continent$variable)


ui <- fluidPage(

    titlePanel("The gapminder dataset"),
    
    sidebarLayout(
        sidebarPanel(
          selectInput("continent", 
                      "Select a continent", 
                      choices = continents,
                      selected = "Europe"),
          
          selectInput("variable", 
                      "Select a variable*", 
                      choices = variables,
                      selected = "lifeExp"),
          
          helpText("*Mean value.")
        ),
        
        mainPanel(
          plotlyOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  # Filtered data frame is a reactive object
  df_plot <- reactive({
    
    # Filter according to user inputs
    df_continent %>% 
      filter(continent == input$continent & variable == input$variable)
    
  })

  output$plot <- renderPlotly({
    
    # Plot the reactive object df_plot()
    p <- plot_ly() %>%
      add_trace(x = df_plot()$year,
                y = df_plot()$value,
                type = "scatter",
                mode = "lines",
                line = list(color = "blue")) %>% 
      layout(xaxis = list(dtick = 5,
                          tickangle = -45),
             hovermode = "x",
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.15)) %>%
      config(displayModeBar = F)
    
  })
  
}

shinyApp(ui = ui, server = server)