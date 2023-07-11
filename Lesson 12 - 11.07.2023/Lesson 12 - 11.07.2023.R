rm(list = ls())

library(dplyr)
library(plotly)
library(openxlsx)
library(tidyverse)
library(rdbnomics)

country_code <- c("ITA", "FRA", "ESP", "DEU")

# Filter function to clean the data after pull from DBnomics
filter_function <- function(df_x) {
  
  df_x_filtered <- df_x %>% 
    select("Country",
           "original_period",
           "value") %>% 
    rename(Year = original_period) %>% 
    filter(Year >= 1995 & Year <= 2022)
  
  return(df_x_filtered)
  
}

# Gross domestic product at current prices [UVGD]
df_GDP <- rdb(ids = paste0("AMECO/UVGD/", country_code, ".1.0.99.0.UVGD")) %>% 
  filter_function() %>% 
  rename(GDP = value)

# Net lending (+) or net borrowing (-): general government :- ESA 2010 [UBLG]
df_FSg <- rdb(ids = paste0("AMECO/UBLG/", country_code, ".1.0.319.0.UBLG")) %>% 
  filter_function() %>% 
  rename(Government = value)

# Balance on current transactions with the rest of the world (National accounts) [UBCA]
df_FSf <- rdb(ids = paste0("AMECO/UBCA/", country_code, ".1.0.310.0.UBCA")) %>% 
  filter_function() %>% 
  rename(Foreign = value)

# Net lending (+) or net borrowing (-): households and NPISH [UBLH]
df_FSh <- rdb(ids = paste0("AMECO/UBLH/", country_code, ".1.0.99.0.UBLH")) %>% 
  filter_function() %>% 
  full_join(df_GDP, by = c("Year", "Country")) %>% 
  mutate(Households = value / GDP * 100)

# Net lending (+) or net borrowing (-): corporations [UBLC]
df_FSc <- rdb(ids = paste0("AMECO/UBLC/", country_code, ".1.0.99.0.UBLC")) %>% 
  filter_function() %>% 
  full_join(df_GDP, by = c("Year", "Country")) %>% 
  mutate(Corporations = value / GDP * 100)

# Joining data
data_FS <- df_FSg %>% 
  full_join(df_FSf, by = c("Year", "Country")) %>% 
  full_join(df_FSh, by = c("Year", "Country")) %>%
  full_join(df_FSc, by = c("Year", "Country")) %>% 
  select(Country, Year, Government, Foreign, Households, Corporations)

# Start excel file
my_data <- createWorkbook()

# Add sheet
addWorksheet(my_data, sheetName = "SFB") 

# Write sheet
writeData(my_data, 1, x = data_FS)

# Save file
saveWorkbook(my_data, 
             file = "Lesson 12 - 11.07.2023/data_FS.xlsx", 
             overwrite = TRUE)

# Sectoral financial balances stacked bar chart
p <- plot_ly() %>%
  add_trace(x = data_FS$Year[data_FS$Country == "Italy"],
            y = round(data_FS$Government[data_FS$Country == "Italy"], digits = 2),
            type = "bar",
            name = "Government sector",
            marker = list(color = "green"),
            hoverlabel = list(namelength = -1)) %>%
  add_trace(x = data_FS$Year[data_FS$Country == "Italy"],
            y = -round(data_FS$Foreign[data_FS$Country == "Italy"], digits = 2),
            type = "bar",
            name = "Foreign sector",
            hoverlabel = list(namelength = -1),
            marker = list(color = "red")) %>%
  add_trace(x = data_FS$Year[data_FS$Country == "Italy"],
            y = round(data_FS$Households[data_FS$Country == "Italy"], digits = 2),
            type = "bar",
            name = "Household sector",
            hoverlabel = list(namelength = -1),
            marker = list(color = "blue")) %>%
  add_trace(x = data_FS$Year[data_FS$Country == "Italy"],
            y = round(data_FS$Corporations[data_FS$Country == "Italy"], digits = 2),
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
