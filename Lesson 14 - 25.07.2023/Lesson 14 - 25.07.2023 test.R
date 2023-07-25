library(gapminder)
library(dplyr)

data_2007 <- gapminder %>% 
  filter(year == 2007)

robust_scalar <- function(x) {
  (x - median(x)) / IQR(x)
}

minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_2007_scaled <- data_2007 %>% 
  mutate(across(where(is.numeric), minmax))

plot(as.data.frame(data_2007_scaled[, -c(1,2,3)]))

fitK <- kmeans(data_2007_scaled[, -c(1,2,3)], 4)

fitK

str(fitK)

fitK$cluster

plot(data_2007_scaled[, -c(1,2,3)], col = fitK$cluster)
