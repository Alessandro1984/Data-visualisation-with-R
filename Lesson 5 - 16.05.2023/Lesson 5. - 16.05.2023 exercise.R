rm(list = ls())

x <- c(	68, 84, 75, 82, 68, 90, 62, 88, 76, 93,
        73, 79, 88, 73, 60, 93, 71, 59, 85, 75,
        61, 65, 75, 87, 74, 62, 95, 78, 63, 72,
        66, 78, 82, 75, 94, 77, 69, 74, 68, 60,
        96, 78, 89, 61, 75, 95, 60, 79, 83, 71,
        79, 62, 67, 97, 78, 85, 76, 65, 71, 75,
        65, 80, 73, 57, 88, 78, 62, 76, 53, 74,
        86, 67, 73, 81, 72, 63, 76, 75, 85, 77)

# 1) The highest grade 
max(x)

# 2) The lowest grade
min(x)

# 3) The range
max(x) - min(x)

# 4) The grades of the five highest ranking students
x_sorted <- sort(x)
x_sorted[76:80]

# 5) The grades of the five lowest ranking students
x_sorted[1:5]

# 6) The grade of the tenth high ranking student
x_sorted[71]

# 7) The number of students who received grades of 75 or higher
sum(x >= 75)

# 8) The number of students who received grades below 85
sum(x < 85)

# 9) The percentage of students who received grades higher than 65 but not higher than 85
sum(x > 65 & x <= 85) / length(x) * 100

# 10) Produce an histogram of the data
hist(x,
     ylim = c(0, 20),
     main = "Final grades in Mathematics (N = 80)",
     xlab = "Grades")
