df_new <- data.frame(
  x = rnorm(100, 0, 1),
  y = rnorm(100, 0, 1),
  z = rnorm(100, 0, 1)
)

#plot(df_new)

# Function to compute total within-cluster sum of square 
wss1 <- function(k) {
  kmeans(df_scaled, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 9
k.values <- 1:9

# Extract wss for 1-9 clusters
wss_values1 <- map_dbl(k.values, wss1)

par(mfrow = c(1,2))

# Plot elbow
plot(k.values, wss_values1,
     type = "b", 
     pch = 19,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
axis(1, at = seq(1, 9, by = 1))

# Function to compute total within-cluster sum of square 
wss2 <- function(k) {
  kmeans(df_new, k, nstart = 10)$tot.withinss
}

# Extract wss for 1-9 clusters
wss_values2 <- map_dbl(k.values, wss2)

# Plot elbow
plot(k.values, wss_values2,
     type = "b", 
     pch = 19,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
axis(1, at = seq(1, 9, by = 1))
