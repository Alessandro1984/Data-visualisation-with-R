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
           plotlyOutput("plot"),
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
    
    selected_country <- input$country
    
    cols <- c("Maximum" = "blue", 
              "Minimum" = "red",
              "Median" = "orange",
              "China" = "black")
    
    p1 <- ggplot(df_plot_final()) +
      geom_line(aes(x = year, 
                    y = max_value,
                    colour = "Maximum"), 
                linetype = "dashed",
                linewidth = 1) +
      geom_line(aes(x = year, 
                    y = min_value,
                    colour = "Minimum"), 
                linetype = "dashed",
                linewidth = 1) +
      geom_line(aes(x = year, 
                    y = med_value,
                    colour = "Median"), 
                linetype = "dashed",
                linewidth = 1) +
      geom_line(aes(x = year, 
                    y = lifeExp_country,
                    colour = "China"),
                linewidth = 1) +
      theme_bw() + 
      labs(color = NULL,
           y = "Life expectancy at birth",
           caption = "Source: Gapminder.") +
      scale_color_manual(values = cols) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45,
                                       hjust = 1),
            axis.title.x = element_blank())
    
    p_plotly <- ggplotly(p1) %>% 
      config(displayModeBar = F)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)