# Insert mean and std deviation into A, B & C.  Add other variables, as needed.

A <- rnorm(10000, 83, 2.5)
B <- rnorm(10000, 33.10, 2.5)
#C <- rnorm(10000, 3, 0.2)

# Create an equation:

Eqn <- (A - B)*100/A

# Calc means and sd for composite value:

Em <- mean(Eqn)
Esd <- sd(Eqn)
k <- 1.98
E.mu <- k*Esd

Em
Esd
k
E.mu

hist(Eqn, breaks=30)
