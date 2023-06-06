rm(list = ls())

library(here)
library(dplyr)
library(ggplot2)
library(patchwork)

df_final_modified <- readRDS(here("Lesson 7 - 30.05.2023/df_final_modified.rds"))

countries <- unique(df_final_modified$Country)

df_stats <- data.frame(Country = countries,
                       slopes = numeric(length(countries)),
                       r_squared = numeric(length(countries)))

for (c in 1:length(countries)) {
  
  reg <- lm(dunemp ~ rgdp_gr,
            data = df_final_modified[df_final_modified$Country == countries[c],])
  
  df_stats$slopes[c] <- round(reg$coefficients[2], digits = 2)
  df_stats$r_squared[c] <- round(summary(reg)$r.squared, digits = 2) * 100
  
}

df_stats_slopes <- df_stats %>%
  mutate(abs_slopes = abs(slopes))

cols <- c("Germany" = "blue",
          "Spain" = "red",
          "France" = "green",
          "United States" = "orange",
          "Italy" = "black",
          "Netherlands" = "purple")

p1 <- ggplot(df_stats_slopes, aes(x = reorder(Country, -abs_slopes),
                                  y = abs_slopes,
                                  fill = Country)) +
  scale_fill_manual(values = cols) +
  geom_col() +
  theme_bw() +
  labs(y = "Slope coefficient, percent \n (absolute value)") +
  geom_text(aes(label = abs_slopes),
            vjust = 1.6,
            color = "white",
            size = 3.5) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        axis.title.x = element_blank())

p1

p2 <- ggplot(df_stats_slopes, aes(x = reorder(Country, -r_squared),
                                  y = r_squared,
                                  fill = Country)) +
  scale_fill_manual(values = cols) +
  geom_col() +
  theme_bw() +
  labs(y = "R-Squared, percent") +
  geom_text(aes(label = paste0(r_squared, "%")),
            vjust = 1.6,
            color = "white",
            size = 3.5) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        axis.title.x = element_blank())

p2

# Using the patchwork package
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
