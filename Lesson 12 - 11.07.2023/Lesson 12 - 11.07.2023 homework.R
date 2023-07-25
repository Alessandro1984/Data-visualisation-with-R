library(plotly)

set.seed(348546)

n_periods <- 50
n_scenarios <- 30
drift <- 2
variation <- 1
initial_value <- 0

p <- plot_ly() %>% 
  add_trace(x = c(0, n_periods),
            y = c(initial_value, initial_value + n_periods*drift),
            type = "scatter",
            mode = "lines",
            hoverinfo = "none",
            line = list(color = "black")) %>% 
  config(displayModeBar = F)

for (r in 1:n_scenarios) {
  
  e <- rnorm(n_periods, drift, variation)
  
  y <- initial_value + cumsum(e)
  
  p <- p %>%
    add_trace(x = 1:n_periods,
              y = y,
              type = "scatter",
              mode = "lines",
              name = paste0("Random walk ", r),
              showlegend = F,
              line = list(color = "lightgrey"))
  
}

p