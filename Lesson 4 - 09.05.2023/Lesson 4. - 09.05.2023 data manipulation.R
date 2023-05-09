# Reference manual for the package 
# https://cloud.r-project.org/web/packages/wooldridge/wooldridge.pdf
library(wooldridge)

rm(list = ls())

# Structure of the dataset ----

# We first take a look at the dataset
View(wage1)

# How many rows in the dataset?
nrow(wage1)

# How many columns?
ncol(wage1)

# How many columns and rows?
dim(wage1)

# Display the internal structure
str(wage1)

# Names of the columns
colnames(wage1)

# Indexing ----

# Useful when you need to address a particular element of a vector, 
# for example the years of education of the fifth worker in the dataset
wage1$educ[5]

# If we want data for more than one worker
wage1$educ[c(2,3,5)]

# Data from worker one to worker five
wage1$educ[1:5]

# If we want to modify one particular observation
wage1$educ[5] <- NA

# Negative indexing allows to show data except those specified in parenthesis
wage1$educ[-c(5:526)]

# Negative indexing allows to drop specific rows and columns. 
# Here we drop the first row of the dataset
wage1[-1,]

# Here we we drop the first column of the dataset
wage1[,-1]

# We can also keep specific column selecting them by name
wage1[, colnames(wage1) %in% c("wage", "educ", "exper")]

# We convert dollars to euros and add the new column to the dataset
wage1$wage_EUR <- wage1$wage * 0.86

# We can compare the variable wage, originally expressed in dollars, 
# and the new variable `wage_EUR`. Here we take a look at the first five rows
wage1[1:5, c("wage", "wage_EUR")]

# Conditional selection ----

# To be used when you need to extract data that satisfy certain criteria 
# Workers that have more than 15 years of education
wage1$educ[wage1$educ > 15]

# How many workers have more than 15 years of education?
length(wage1$educ[wage1$educ > 15])

# How many workers have education between 15 and 18 years of education?
length(wage1$educ[wage1$educ >= 15 & wage1$educ <= 18])

# What is the mean wage of workers that have between 15 and 18 
# years of education?
mean(wage1$educ[wage1$educ >= 15 & wage1$educ <= 18])

# What is the percentage of workers who have more than 15 years of experience?

# Correlation matrix ----
rm(list = ls())
library(corrplot)

data_W <- wage1[, c("wage", "educ", "exper", "tenure")]

data_W_corr <- cor(data_W)

corrplot(data_W_corr, 
         method = "number", 
         type = "upper")
