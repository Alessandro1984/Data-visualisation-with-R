rm(list = ls())

library(dplyr)
library(ggplot2)
library(rdbnomics)

df_usa <- rdb(ids = "AMECO/ZUTN/USA.1.0.0.0.ZUTN") %>% 
  select(Country, value, original_period) %>% 
  rename(Year = original_period) %>%
  filter(Year <= 2022) %>% 
  mutate(Year = as.numeric(Year))

p1 <- ggplot(df_usa, aes(x = Year, 
                         y = value,
                         color = Country)) +
  geom_line() +
  theme_bw() +
  labs(y = "Percent",
       title = paste0("Unemployment rate,", min(df_usa$Year), "-", max(df_usa$Year)),
       subtitle = "Definition EUROSTAT (ZUTN)",
       caption = "Source: AMECO data from dbnomics.") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous(breaks = seq(min(df_usa$Year), max(df_usa$Year), 5))

p1

# saveRDS(df_usa, file = "Lesson 6. - 23.05.2023/df_usa.rds")

# readRDS(file = "Lesson 6. - 23.05.2023/df_usa.rds")

country_code <- c("D_W", "DEU", "FRA", "ITA", "NLD", "ESP", "USA")

countries <- paste0("AMECO/ZUTN/", country_code, ".1.0.0.0.ZUTN")

df <- rdb(ids = countries) %>% 
  select(Country, value, original_period) %>% 
  rename(Year = original_period) %>% 
  filter(Year <= 2022) %>% 
  mutate(Year = as.numeric(Year))

df_west_germany <- df %>% 
  filter(Country == "West Germany" & Year < 1991)

df_germany <- df %>% 
  filter(Country == "Germany" & Year >= 1991)

df_germany_together <- df_west_germany %>% 
  bind_rows(df_germany) %>% 
  mutate(Country = recode(Country, `West Germany` = "Germany"))

df_final <- df %>% 
  filter(!(Country %in% c("West Germany", "Germany"))) %>% 
  bind_rows(df_germany_together)

cols <- c("Germany" = "blue", 
          "Spain" = "red", 
          "France" = "green", 
          "United States" = "orange", 
          "Italy" = "black", 
          "Netherlands" = "purple")

p2 <- ggplot(df_final, aes(x = Year, 
                           y = value,
                           color = Country)) +
  scale_color_manual(values = cols) +
  geom_line() +
  theme_bw() +
  labs(y = "Percent",
       title = paste0("Unemployment rate,", min(df_final$Year), "-", max(df_final$Year)),
       subtitle = "Definition EUROSTAT (ZUTN)",
       caption = "Source: AMECO data from dbnomics.") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous(breaks = seq(min(df_final$Year), max(df_final$Year), 5))

p2

# saveRDS(df_final, file = "Lesson 6. - 23.05.2023/df_final.rds")

# readRDS(file = "Lesson 6. - 23.05.2023/df_final.rds")
