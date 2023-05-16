library(wooldridge)

wage1_new <- wage1[,c("wage", "female", "married")]

wage1_male_married <- round(mean(wage1_new$wage[c(wage1_new$married == 1 & wage1_new$female == 0)]), digits = 2)
wage1_male_single <- round(mean(wage1_new$wage[c(wage1_new$married == 0 & wage1_new$female == 0)]), digits = 2)
wage1_female_married <- round(mean(wage1_new$wage[c(wage1_new$married == 1 & wage1_new$female == 1)]), digits = 2)
wage1_female_single <- round(mean(wage1_new$wage[c(wage1_new$married == 0 & wage1_new$female == 1)]), digits = 2)

gender <- c("Male", "Female")

marital_status <- c("Married", "Single")

wage1_new$gender <- ifelse(wage1_new$female == 0, "Male", "Female")

wage1_new$marital_status <- ifelse(wage1_new$married == 0, "Single", "Married")

means <- matrix(nrow = 2, ncol = 2)

rownames(means) <- gender

colnames(means) <- marital_status

for (g in 1:length(gender)) {
  
  for (m in 1:length(marital_status)) {
    
    means[g, m] <- round(mean(wage1_new$wage[c(wage1_new$marital_status == marital_status[m] & wage1_new$gender == gender[g])]), digits = 2)

  }
  
}

barplot(sort(as.vector(means), decreasing = TRUE),
        ylim = c(0, 10),
        main = "Mean hourly wage by gender and marital status",
        ylab = "Dollar per hour",
        col = c("blue", "blue", "red", "red"),
        names.arg = c("Married \n men", "Single \n men", "Single \n women", "Married \n women"))
