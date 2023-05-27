library(dplyr)

filter_function <- function(df_x) {
  
  df_final <- df_x %>% 
  select(Country, value, original_period) %>% 
    rename(Year = original_period) %>% 
    filter(Year <= 2022) %>% 
    mutate(Year = as.numeric(Year))
  
  return(df_final)
  
}

germany_function <- function(df_x) {

df_west_germany <- df_x %>% 
  filter(Country == "West Germany" & Year < 1991)

df_germany <- df_x %>% 
  filter(Country == "Germany" & Year >= 1991)

df_germany_together <- df_west_germany %>% 
  bind_rows(df_germany) %>% 
  mutate(Country = recode(Country, `West Germany` = "Germany"))

df_final <- df_x %>% 
  filter(!(Country %in% c("West Germany", "Germany"))) %>% 
  bind_rows(df_germany_together)

return(df_final)

}

