
item=c('Sofas ','couches','Bookshelves','armchairs','Desks','Dressers','wardrobes','Entertainment centers','Dinning Tables','Beds ')
weights = c(70,73,77,80,82,87,90,94,98,106)
values = c(135,139,149,150,156,163,173,184,192,201) 
max_weight=750


# Define the objective function
knapsack <- function(x) {
  # x is a binary vector indicating whether each item is selected or not
  if (sum(weights * x) > max_weight) {
    # If the weight limit is exceeded, return a very negative value
    return(-Inf)
  } else {
    # Otherwise, return the total value of the selected items
    return(sum(values * x))
  }
}
# Define the simulated annealing function
simulated_annealing <- function(f, x0, T0, alpha, L, maxiter) {

  # Initialize the objective function value vector
  fx_vec <- numeric(maxiter)
  
  x <- x0
  fx <- f(x)
  T <- T0
  
  for (iter in 1:maxiter) {
    for (i in 1:L) {
      # Generate a random neighbor
      xn <- sample(c(0, 1), length(x), replace = TRUE)
      # Calculate the objective function value of the neighbor
      fxn <- f(xn)
      # Calculate the change in objective function value
      deltaf <- fxn - fx
      # Decide whether to accept the neighbor
      if (deltaf > 0) {
        # Accept the neighbor if it improves the objective function value
        x <- xn
        fx <- fxn
      } else {
        # Accept the neighbor with probability exp(deltaf/T)
        if (runif(1) < exp(deltaf/T)) {
          x <- xn
          fx <- fxn
        }
      }
    }
    # Update the temperature
    T <- alpha * T
    
    # Store the objective function value at each iteration
    fx_vec[iter] <- fx
  }
  
  # Return the best solution found and the objective function value vector
  return(list(x = x, fx_vec = fx_vec))
}


# Set the initial temperature and cooling schedule parameter
T0 <- 1
alpha <- 0.95

# Run the simulated annealing algorithm
set.seed(123)  # Set the random seed for reproducibility
x0 <- sample(c(0, 1), length(weights), replace = TRUE)  # Generate a random initial solution
#x_best <- simulated_annealing(knapsack, x0, T0, alpha, L = 100, maxiter = 1000)
sa_result <- simulated_annealing(knapsack, x0, T0, alpha, L = 100, maxiter = 30)


# Print the results
cat("Selected items:", sa_result$x, "\n")
cat("Total weight:", sum(weights * sa_result$x), "\n")
cat("Total value:", sum(values * sa_result$x), "\n")



# Create a convergence plot
plot(1:length(sa_result$fx_vec), sa_result$fx_vec, type = "l",
     xlab = "Generation", ylab = "Objective function value",
     main = " ", col="blue")

