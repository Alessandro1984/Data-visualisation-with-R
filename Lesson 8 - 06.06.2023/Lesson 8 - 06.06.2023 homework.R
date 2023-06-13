rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)

cols <- c("Maximum" = "blue", 
          "Minimum" = "red",
          "Median" = "orange",
          "China" = "black")


# First solution ----
df_plot <- gapminder %>% 
  group_by(year) %>% 
  summarise(min_value = min(lifeExp), 
            max_value = max(lifeExp),
            med_value = median(lifeExp))

df_plot_China <- gapminder %>%
  filter(country == "China") %>% 
  select(year, lifeExp) %>% 
  rename(lifeExp_China = lifeExp)

df_plot_final <- df_plot %>% 
  inner_join(df_plot_China, by = "year")

p1 <- ggplot(df_plot_final) +
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
                y = lifeExp_China,
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

p1

# Second solution ----
df_plot_final_long <- df_plot_final %>% 
# From wide to long format
  pivot_longer(!year, names_to = "variable", values_to = "value") %>% 
# Renaming with case_match() within mutate
  mutate(variable = case_match(
    variable,
    "max_value" ~ "Maximum", 
    "min_value" ~ "Minimum",
    "med_value" ~ "Median",
    "lifeExp_China" ~ "China"))

p2 <- ggplot(df_plot_final_long, aes(x = year, 
                                     y = value,
                                     color = variable)) + 
  geom_line(aes(linetype = variable), linewidth = 1) +
# We must include guide = "none" to remove the legend of the second aesthetics, linetype 
  scale_linetype_manual(guide = "none",
                        values = c("China" = "solid", 
                                   "Minimum" = "dashed",
                                   "Maximum" = "dashed",
                                   "Median" = "dashed"))+
  scale_color_manual(values = cols) + 
  theme_bw() + 
  labs(color = NULL,
       y = "Life expectancy at birth",
       caption = "Source: Gapminder.") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        axis.title.x = element_blank())
p2
