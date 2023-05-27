rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(rdbnomics)

source("Lesson 7 - 30.05.2023/functions.r")

country_code <- c("D_W", "DEU", "FRA", "ITA", "NLD", "ESP", "USA")

countries <- paste0("AMECO/OVGD/", country_code, ".1.1.0.0.OVGD")

df_rgdp <- rdb(ids = countries) %>% 
  filter_function() %>% 
  germany_function()

df_unemp <- readRDS("Lesson 6. - 23.05.2023/df_final.rds")

df_final <- df_rgdp %>% 
  inner_join(df_unemp, by = c("Country", "Year")) %>% 
  rename(rgdp = value.x,
         unemp = value.y) %>% 
  relocate(Year, Country, rgdp, unemp) %>% 
  group_by(Country) %>% 
  mutate(dunemp = c(NA, diff(unemp)),
         rgdp_gr = round(c(NA, diff(rgdp))/lag(rgdp) * 100, digits = 2)) %>% 
  drop_na()

cols <- c("Germany" = "blue", 
          "Spain" = "red", 
          "France" = "green", 
          "United States" = "orange", 
          "Italy" = "black", 
          "Netherlands" = "purple")

p1 <- ggplot(df_final, aes(y = rgdp_gr, 
                           x = Year, 
                           group = Country, 
                           color = Country)) +
  geom_line() + 
  theme_bw() +
  scale_color_manual(values = cols) +
  theme(legend.position = "bottom") + 
  labs(color = NULL,
       y = "Percent",
       title = paste0("Gross domestic product,", min(df_final$Year), "-", max(df_final$Year)),
       subtitle = "2015 reference levels (OVGD)",
       caption = "Source: AMECO data from dbnomics.") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous(breaks = seq(min(df_final$Year), max(df_final$Year), 5))

p1

df_final_modified <- df_final %>% 
  filter(!(Year %in% c(1991, 2020:2022)))

# saveRDS(df_final_modified, file = "Lesson 7 - 30.05.2023/df_final_modified.rds")

p2 <- ggplot(df_final_modified, aes(x = rgdp_gr, 
                                    y = dunemp, 
                                    group = Country, 
                                    color = Country)) +
  geom_point() + 
  theme_bw() +
  geom_smooth(method = lm, 
              se = FALSE, 
              fullrange = TRUE) +
  scale_color_manual(values = cols) +
  theme(legend.position = "bottom") + 
  labs(color = NULL,
       y = "Diff. unemployment rate, \n percentage points",
       x = "Growth rate real gdp, \n percent",
       title = paste0("Okun’s law,", min(df_final$Year), "-", max(df_final$Year)),
       caption = "Source: AMECO data from dbnomics.")

p2

p3 <- ggplot(df_final_modified, aes(x = rgdp_gr, 
                                    y = dunemp, 
                                    group = Country, 
                                    color = Country)) +
  geom_point() + 
  theme_bw() +
  geom_smooth(method = lm, 
              se = FALSE, 
              fullrange = TRUE) +
  scale_color_manual(values = cols) +
  facet_wrap(~Country, 
             nrow = 2, 
             strip.position = "top", 
             scales = "free") +
  theme(legend.position = "bottom",
        strip.background = element_blank(), 
        strip.placement = "outside") +
  labs(color = NULL,
       y = "Diff. unemployment rate, \n percentage points",
       x = "Growth rate real gdp, \n percent",
       title = paste0("Okun’s law,", min(df_final$Year), "-", max(df_final$Year)),
       caption = "Source: AMECO data from dbnomics.")

p3

