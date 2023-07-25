library(dplyr)
library(purrr)

str(iris)

# Drop the species column
df <- iris[, -5]

# We can try different scaling functions
# There is actually no need to scale data here. Data are expressed in the same unit
standard_scalar <- function(x) {
  (x - mean(x)) / sd(x)
}

robust_scalar <- function(x) {
  (x - median(x)) / IQR(x)
}

minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Scale data
df_scaled <- df %>% 
  mutate(across(everything(), standard_scalar))

# Plot scaled data
plot(as.data.frame(df_scaled))

# Check number of clusters
# Source: https://uc-r.github.io/kmeans_clustering

# Function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df_scaled, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 9
k.values <- 1:9

# Extract wss for 1-9 clusters
wss_values <- map_dbl(k.values, wss)

# Plot elbow
plot(k.values, wss_values,
     type = "b", 
     pch = 19,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
axis(1, at = seq(1, 9, by = 1))

# Fit clusters
fitK <- kmeans(df_scaled, 2)

# What is saved in fitK?
fitK

# Let's plot again the data with color corresponding to the different clusters
plot(df_scaled, col = fitK$cluster) 

# Apparently the third type of iris is difficult to detect..

