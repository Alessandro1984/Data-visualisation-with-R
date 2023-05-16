rm(list = ls())

library(wooldridge)

northcen <- sum(wage1$northcen[wage1$northce == 1])
south <- sum(wage1$south[wage1$south == 1])
west <- sum(wage1$west[wage1$wes == 1])
east <- nrow(wage1) - northcen - south - west

slices <- c(northcen, south, west, east)

slices_pct <- round(slices/sum(slices)*100)

labels <- c("North Center", "South", "West", "East")

pie(slices, 
    labels = paste0(slices_pct, "%"), 
    main = paste0("Workers by region (N = ", sum(slices), ")"),
    col = rainbow(length(slices)))

legend("bottomright", 
       labels, 
       cex = 0.8,
       bty = "n",
       fill = rainbow(length(slices)))
