rm(list = ls())

library(dplyr)
library(ggplot2)
library(rdbnomics)

df_final <- readRDS(file = "Lesson 6. - 23.05.2023/df_final.rds")

cols <- c("Germany" = "blue", 
          "Spain" = "red", 
          "France" = "green", 
          "United States" = "orange", 
          "Italy" = "black", 
          "Netherlands" = "purple")

p3 <- ggplot(df_final, aes(x = Year, 
                           y = value,
                           color = Country)) +
  geom_line() +
  theme_bw() +
  labs(y = "Percent") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_color_manual(values = cols) +
  facet_wrap(~Country, 
             nrow = 2,
             scales = "free") +
  scale_x_continuous(breaks = seq(min(df_final$Year), max(df_final$Year), 10)) + 
  theme(strip.background = element_blank())

p3
