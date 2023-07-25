library(ggplot2)
library(eurostat)

# Info: https://ec.europa.eu/eurostat/cache/metadata/en/gov_10a_exp_esms.htm

# Remove '#' and execute to download data
# COFOG <- get_eurostat("gov_10a_exp", cache = TRUE)

# Remove '#' and change to your favorite directory (or just save in the wd)
# saveRDS(COFOG, file = "Lesson 14 - 25.07.2023/COFOG.rds")

# Change path to file
COFOG <- readRDS(file = "Lesson 14 - 25.07.2023/COFOG.rds") 

df_total_exp <- COFOG %>% 
  filter(geo %in% c("DE", "IT", "FR", "ES"),
         unit == "PC_GDP",
         cofog99 == "TOTAL",
         na_item == "TE",
         sector == "S13")

cols <- c("DE" = "blue", 
          "ES" = "red", 
          "FR" = "green",
          "IT" = "black")

p <- ggplot(df_total_exp,aes(x = time, 
                              y = values,
                              color = geo)) +
  scale_color_manual(values = cols) +
  geom_line() +
  theme_bw() +
  labs(y = "Percent",
       title = "Total government spending in percent of GDP",
       caption = "Source: EUROSTAT.") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

p
