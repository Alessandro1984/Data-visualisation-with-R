library(wooldridge)

wage1$gender <- ifelse(wage1$female == 0, "Male", "Female")

cols <- c("blue", "red")

gender <- c("Male", "Female")

# Function to close the current graphics device
# dev.off()

par(mfrow = c(1, 2))

plot(NULL,
     type = "n",
     xlim = c(min(wage1$educ), max(wage1$educ)), 
     ylim = c(min(wage1$wage), max(wage1$wage)),
     xlab = "Years of education", 
     ylab = "Dollar per hour")

for (i in 1:length(gender)) {
  
  points(x = wage1$educ[wage1$gender == gender[i]],
         y = wage1$wage[wage1$gender == gender[i]],
         pch = i,
         col = cols[i])
  
}

legend("topleft",
       legend = gender,
       col = cols, 
       bty = "n",
       pch = c(1,2),
       cex = 1)

boxplot(wage1$wage[wage1$female == 0],
        wage1$wage[wage1$female == 1],
        horizontal = FALSE,
        ylab = "Dollar per Hour",
        xlab = "Gender",
        names = c("Male", "Female"),
        col = c("blue", "red"))
