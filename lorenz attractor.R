library(deSolve)
library(rgl)

# Lorenz system parameters
sigma <- 10
rho <- 28
beta <- 8/3

# Function to calculate Lorenz system equations
lorenz <- function(t, state, parameters) {
  x <- state[1]
  y <- state[2]
  z <- state[3]
  
  dx <- sigma * (y - x)
  dy <- x * (rho - z) - y
  dz <- x * y - beta * z
  
  return(list(c(dx, dy, dz)))
}

# Initial state and time steps
state <- c(x = 1, y = 1, z = 1)
t <- seq(0, 50, by = 0.01)

# Solve the differential equations
solution <- ode(y = state, times = t, func = lorenz, parms = NULL, method = "lsoda")

# Extract x, y, z coordinates
x <- solution[, "x"]
y <- solution[, "y"]
z <- solution[, "z"]
# Create color gradient based on x dimension position
x_min <- min(x)
x_max <- max(x)
normalized_x <- (x - x_min) / (x_max - x_min)

# Define color interpolation function
interpolate_color <- function(x) {
  if (!is.na(x)){
  red <- x
  green <- abs(x - 0.5)
  blue <- 1 - x
  color <- rgb(red, green, blue, maxColorValue = 1)
  return(color)}
  else{
    return(rgb(0,0,0))
  }
}
R.Version()
# Generate colors based on x dimension position
color <- sapply(normalized_x, interpolate_color)

# Plot the Lorenz attractor with color gradient
open3d()
plot3d(x, y, z, type = "l", col = color, size = 0.8, xlab = "X", ylab = "Y", zlab = "Z")
