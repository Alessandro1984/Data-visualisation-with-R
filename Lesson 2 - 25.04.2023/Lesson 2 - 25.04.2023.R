library(readxl)

data_gdp <- read_excel("Lesson 2 - 25.04.2023/data_gdp.xlsx", 
                       sheet = "data", skip = 2)

countries <- c("France", "Germany", "Italy", "Spain")
cols <- c("blue", "red", "green", "orange")

plot(NULL,
     xlim = c(1960, 2022),
     ylim = c(0, 4000),
     main = "Gross domestic product, 1960-2022",
     xlab = "Year",
     ylab = "Mrd. Euro")

for (c in 1:length(countries)) {
  
  lines(x = data_gdp$year[data_gdp$country == countries[c]],
        y = data_gdp$value[data_gdp$country == countries[c]],
        type = "l",
        col = cols[c])
  
}

legend("topleft",
       legend = countries,
       col = cols, 
       lty = 1, 
       lwd = 1, 
       bty = "n", 
       cex = 0.8)

par(mfrow = c(2, 2))

for (i in 1:length(countries)) {
  
  plot(NULL,
       xlim = c(1960, 2022),
       ylim = c(min(data_gdp$value[data_gdp$country == countries[i]]), 
                max(data_gdp$value[data_gdp$country == countries[i]])),
       main = countries[i],
       xlab = "Year",
       ylab = "Mrd. Euro")
  
  lines(x = data_gdp$year[data_gdp$country == countries[i]],
        y = data_gdp$value[data_gdp$country == countries[i]],
        type = "l",
        col = cols[i])
}
