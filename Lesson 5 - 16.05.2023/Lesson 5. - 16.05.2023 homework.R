library(readxl)

data_real_gdp <- read_excel("data_real_gdp.xlsx", 
                            sheet = "data", 
                            skip = 3)

countries <- unique(data_real_gdp$country)

cols <- c("blue", "red", "green", "orange", "black", "purple")

par(mfrow = c(2, 3))

data_real_gdp$time_trend <- rep(1:length(data_real_gdp$year[data_real_gdp$country == "Germany"]), 
                                length(countries)) 

for (i in 1:length(countries)) {
  
  plot(x = data_real_gdp$year[data_real_gdp$country == countries[i]],
       y = log(data_real_gdp$value[data_real_gdp$country == countries[i]]),
       xlim = c(1960, 2022),
       ylim = c(min(log(data_real_gdp$value[data_real_gdp$country == countries[i]])), 
                max(log(data_real_gdp$value[data_real_gdp$country == countries[i]]))),
       main = countries[i],
       type = "l",
       xlab = "Year",
       ylab = "ln GDP")
  
  y <- data_real_gdp$value[data_real_gdp$country == countries[i]]
    
  x <- data_real_gdp$time_trend[data_real_gdp$country == countries[i]]
  
  reg <- lm(log(y) ~ x + I(x^2))
  
  lines(data_real_gdp$year[data_real_gdp$country == countries[i]],
        reg$fitted.values,
        col = cols[i],
        lty = 1)
  
  # legend("topleft",
  #        legend = paste0("MPC:",round(reg$coefficients[2], digits = 2)),
  #        col = cols[i], 
  #        lty = 1, 
  #        lwd = 1, 
  #        bty = "n", 
  #        cex = 0.8)
  
  legend("bottomright",                                       
         expression(hat(lnGDP) == hat(beta)["0"] + hat(beta)["1"]%.%t + hat(beta)["2"]%.%t^2),         
         col = cols[i],           
         lty = 1,
         cex = 1,
         bty = "n")
  
}
