library(readxl)

data_mpc <- read_excel("Lesson 3 - 02.05.2023/data_mpc.xlsx", 
                       sheet = 1, skip = 1)

countries <- unique(data_mpc$country)

cols <- cols <- c("blue", "red", "green", "orange", "black", "purple")

par(mfrow = c(2, 3))

for (i in 1:length(countries)) {
  
  ratio <- data_mpc$consumption[data_mpc$country == countries[i]] / data_mpc$income[data_mpc$country == countries[i]] * 100
  
  plot(x = data_mpc$year[data_mpc$country == countries[i]],
       y = ratio,
       xlim = c(1991, 2022),
       ylim = c(min(ratio),
                max(ratio)),
       main = countries[i],
       type = "l",
       col = cols[i],
       xlab = "Year",
       ylab = "Percent")
  
}
