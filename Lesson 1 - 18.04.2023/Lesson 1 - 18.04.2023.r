2 + 2

sqrt(25)

exp(0)

log(1)

x <- 15
x

rm(x)

x <- c(1, 2, 3)
x
is.vector(x)

ls()

rm(list = ls())

a <- 1:10
a

b <- seq(from = 1, to = 10, by = 1)

set.seed(1234)
rnorm(n = 10, mean = 0, sd = 1)

length(seq(from = 1, to = 10, by = 0.1))

min(rnorm(n = 100, mean = 0, sd = 10))
max(rnorm(n = 100, mean = 0, sd = 10))

5^2

100 >= 100

99 > 100

2 + 2 == 4

99 != 100

!(1==1) 

(1==1) | (2==3) 

(1==1) & (2==3)

a <- c(1, 2)  
b <- c(1, 2, 3, 4, 5, 6) 
a + b

a <- c(1, 2)  
b <- c(1, 2, 3, 4, 5) 
a + b

x <- vector(mode = "numeric", length = 100)
is.vector(x)

y <- numeric(100)
is.vector(y)

y[1] <- 50

y[1]

x <- 10
y <- 2
if (x <= y) {
  
  print("x is smaller or equal y")
  
} else {
  
  print("x is larger than y")
  
}

x <- rnorm(10, 0, 1)
ifelse(x <= 0, "x is smaller or equal zero", "x is larger than zero")

x <- 1:5
for (i in x) {
  
  print("Hello")
  
}

x <- c("Monday", "Tueasday", "Wednesday", "Thursday", "Friday")

for (i in x) {
  
  print("Hello")
  
}

x <- 1:5

for (i in x) {
  
  print(i^2)
  
}

x <- c("Monday", "Tueasday", "Wednesday", "Thursday", "Friday")

for (i in x) {
  
  print(paste("Today is", i))
  
}

# Formula
# A = P (1 + r)^t
  
years <- 100
A <- numeric(length = years)
r <- 0.01
P <- 1

for (t in 1:years) {
  
  A[t] <- P * (1 + r)^t
  
}

P * (1 + r)^(1:years)

plot(x = 1:years,
     y = A,
     xlim = c(1, years), 
     ylim = c(1, max(A)),
     type = "l", 
     col = "black",
     main = "Compound interests", 
     xlab = "Year", 
     ylab = "Euro")
  
years <- 100
r <- c(0.01, 0.02, 0.03)
A <- matrix(nrow = years, ncol = length(r), byrow = TRUE)
P <- 1

for (t in 1:years) {
  
  for (s in 1:length(r)) {
    
    A[t,s] <- P * (1 + r[s])^t
    
  }
  
}

plot(NULL, 
     xlim = c(1, nrow(A)), 
     ylim = c(1, max(A)),
     main = "Compound interests", 
     xlab = "Year", 
     ylab = "Euro")

for (l in 1:ncol(A)) {
  
  lines(A[,l], type = "l", col = l)
  
}

my_fun <- function(x) {
  
  square <- x^2
  
  return(square)
  
}

my_fun(x = 5)

my_fun_alternative <- function(x) x^2

my_fun_alternative(x = 5)

my_fun2 <- function(x, y) {
  
  to_power <- x^y
  
  return(to_power)
  
}

my_fun2(x = 5, y = 2)

# With default value for y
my_fun2_alternative <- function(x, y = 2) {
  
  to_power <- x^y
  
  return(to_power)
  
}

my_fun2_alternative(x = 5)

my_fun <- function(time, scenarios) {
  
r <- scenarios

A <- matrix(nrow = time, ncol = length(r), byrow = TRUE)

P <- 1

for (t in 1:time) {
  
  for (s in 1:length(r)) {
    
    A[t,s] <- P * (1 + r[s])^t
    
  }
  
}
  
plot(NULL, 
     xlim = c(1, nrow(A)), 
     ylim = c(1, max(A)),
     main = "Compound interests", 
     xlab = "Year", 
     ylab = "Euro")

for (l in 1:ncol(A)) {
  
  lines(A[,l], type = "l", col = l)
  
  }

}

my_fun(time = 10, scenarios = 0.01)

my_fun(time = 10, scenarios = seq(0.01, 0.05, 0.01))

