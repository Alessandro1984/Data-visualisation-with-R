library(gapminder)
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)

# Objects that do not need to be reactive
df_plot <- gapminder %>% 
  group_by(year) %>% 
  summarise(min_value = min(lifeExp), 
            max_value = max(lifeExp),
            med_value = median(lifeExp))

countries <- unique(gapminder$country)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Life expectancy at birth"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("country",
                      "Select a country",
                      choices = countries,
                      selected = "China"),
          helpText("Just some text.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("my_new_plot", width = "500px", height = "400px"),
           # Some notes
           h6("Source: Gapminder.", align = "right", style = "color: red;")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df_plot_final <- reactive({
    
    gapminder %>%
      filter(country == input$country) %>% 
      select(year, lifeExp) %>% 
      rename(lifeExp_country = lifeExp) %>% 
      inner_join(df_plot, by = "year")
    
  })

    output$my_new_plot <- renderPlotly({
      
      p <- plot_ly() %>% 
        add_trace(x = df_plot_final()$year,
                  y = df_plot_final()$max_value,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "blue",
                              dash = "dash"),
                  name = "Maximum") %>% 
        add_trace(x = df_plot_final()$year,
                  y = df_plot_final()$min_value,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "red",
                              dash = "dash"),
                  name = "Minimum") %>% 
        add_trace(x = df_plot_final()$year,
                  y = df_plot_final()$med_value,
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "orange",
                              dash = "dash"),
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
