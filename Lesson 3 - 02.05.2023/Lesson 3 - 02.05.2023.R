library(readxl)

data_mpc <- read_excel("Lesson 3 - 02.05.2023/data_mpc.xlsx", 
                       sheet = 1, skip = 1)

countries <- unique(data_mpc$country)

cols <- c("blue", "red", "green", "orange", "black", "purple")

par(mfrow = c(2, 3))

for (i in 1:length(countries)) {
  
  plot(x = data_mpc$income[data_mpc$country == countries[i]],
       y = data_mpc$consumption[data_mpc$country == countries[i]],
       xlim = c(min(data_mpc$income[data_mpc$country == countries[i]]), 
                max(data_mpc$income[data_mpc$country == countries[i]])),
       ylim = c(min(data_mpc$consumption[data_mpc$country == countries[i]]), 
                max(data_mpc$consumption[data_mpc$country == countries[i]])),
       main = countries[i],
       xlab = "Income",
       ylab = "Consumption")
  
  reg <- lm(data_mpc$consumption[data_mpc$country == countries[i]] ~ 
              data_mpc$income[data_mpc$country == countries[i]])
  
  abline(reg, col = cols[i])
  
  legend("topleft",
         legend = paste0("MPC:",round(reg$coefficients[2], digits = 2)),
         col = cols[i], 
         lty = 1, 
         lwd = 1, 
         bty = "n", 
         cex = 0.8)
  
}
