library(readr)
library(tidyverse)

temp <- list.files(path = "Lesson 13 - 18.07.2023/data_csv", pattern = "*.csv")

myfiles <- lapply(paste0("Lesson 13 - 18.07.2023/data_csv/", temp), read_csv)

data <- myfiles %>% 
  reduce(full_join, by = "Name") %>% 
  select(-contains("Gross"))

old <- names(data)

new <- vector()

new[1] <- "Country"

for (i in 2:20){
  new[i] <- 1999 + i
}

data_new <- data %>% 
  rename_with(~ new, all_of(old)) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "value")

# 20 largest export countries for Germany
top_export <- data_new %>% 
  group_by(Country) %>% 
  summarise(mean_20 = mean(value, na.rm = TRUE)) %>% 
  arrange(desc(mean_20)) %>% 
  slice(1:20)
  
top_export_countries <- top_export$Country
  
data_new_top_export <- data_new %>% 
  filter(Country %in% top_export_countries)

Asia <- c("China", "Japan", "South Korea")

EEC <- c("Poland", "Czechia", "Hungary")

Euro_Area <- c("Germany", "Ireland", "Finland", "Spain", "France", "Italy", "Netherlands", "Austria", "Belgium")

Europe <- c("United Kingdom", "Turkiye", "Denmark", "Russia", "Sweden", "Switzerland")

United_Staes <- c("United States of America")

data_new_top_export$Group <- "United States"
data_new_top_export$Group[data_new_top_export$Country %in% Asia] <-  "Asia"
data_new_top_export$Group[data_new_top_export$Country %in% EEC] <- "EEC"
data_new_top_export$Group[data_new_top_export$Country %in% Euro_Area] <- "Euro Area"
data_new_top_export$Group[data_new_top_export$Country %in% Europe] <- "Europe"

data_new_top_export$Group <- as.factor(data_new_top_export$Group)

saveRDS(data_new_top_export, file = "Lesson 13 - 18.07.2023/data_export.rds")
