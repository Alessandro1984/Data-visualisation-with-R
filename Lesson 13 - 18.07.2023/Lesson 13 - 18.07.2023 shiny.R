library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(gapminder)

data_new_top_export <- readRDS("data_export.rds")

ui <- fluidPage(
  
  titlePanel("Top 20 export destinations for German export, 2001-2019."),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        # First panel
        tabPanel("Year",
                 br(),
      sliderInput("year", 
                  "Select a year", 
                  min = 2001, 
                  max = 2019, 
                  value = 2008,
                  step = 1,
                  sep = "",
                  animate = TRUE)),
      # Second panel
      tabPanel("Info",
               helpText("Values in percent of total export. Source: Atlas of economic complexity."))
    )
  ),
    mainPanel(
      plotlyOutput("plot", height = "700px")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    selection <- input$year
    
    p <- plot_ly() %>% 
      add_trace(y = 0:14,
                x = 0:14,
                type = "scatter",
                mode = "lines",
                hoverinfo = "none",
                showlegend = FALSE,
                line = list(width = 0.5,
                            dash = "dash",
                            color = "black")) %>%
      add_trace(x = data_new_top_export$value[data_new_top_export$Year == 2001], 
                y = data_new_top_export$value[data_new_top_export$Year == selection],
                type = 'scatter', 
                mode = 'markers', 
                hoverlabel = list(namelength = -1),
                hoverinfo = "text",
                text = paste0(data_new_top_export$Country[data_new_top_export$Year == selection], ": ", round(data_new_top_export$value[data_new_top_export$Year == selection], digits = 2),"%"),
                size = data_new_top_export$value[data_new_top_export$Year == selection], 
                color = data_new_top_export$Group[data_new_top_export$Year == selection],
                fill = '', # https://stackoverflow.com/questions/52692760/spurious-warning-when-mapping-marker-size-in-plotly-r
                marker = list(opacity = 0.5,
                              sizemode = 'diameter')) %>%
      layout(xaxis = list(title = "Export share 2001",
                          range = c(-0.1, 14),
                          dtick = 1,
                          tickfont = list(size = 10)),
             yaxis = list(title = paste0("Export share ", selection),
                          dtick = 1,
                          range = c(-0.1, 14),
                          tickfont = list(size = 10)),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.2),
             showlegend = TRUE) %>%
      config(displayModeBar = F) 
    
    p

  })
  
}

shinyApp(ui = ui, server = server)