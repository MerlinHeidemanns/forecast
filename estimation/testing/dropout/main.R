# Load required packages
library(MASS)  # for mvrnorm
library(Matrix)  # for matrix operations

# Function to generate Dirichlet samples
rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  x <- matrix(rgamma(n*k, alpha), nrow=n, byrow=TRUE)
  x/rowSums(x)
}

# Function to create initial correlation structure
create_initial_structure <- function(K) {
  # Generate a random positive definite correlation matrix
  set.seed(123)
  A <- matrix(runif((K+1)^2), K+1, K+1)
  R <- t(A) %*% A
  # Convert to correlation matrix
  D <- diag(1/sqrt(diag(R)))
  R <- D %*% R %*% D
  
  # Get Cholesky decomposition
  L <- chol(R)
  
  # Create scale parameters
  sigma <- runif(K+1, 0.1, 0.5)
  
  # Generate initial vote shares
  p <- c(rdirichlet(1, rep(5, K+1)))
  
  list(R=R, L=L, sigma=sigma, p=p)
}

# Function to handle party dropout
handle_party_dropout <- function(state, j) {
  K <- nrow(state$R) - 1
  r <- K + 1  # residual category index
  
  # Update vote shares
  p_new <- state$p
  p_new[r] <- p_new[r] + p_new[j]
  p_new <- p_new[-j]
  
  # Update correlations for residual category
  rho_new_r <- sapply(1:K, simplify = T, function(i) {
    if(i == j) return(NULL)
    (state$R[i,r] * state$p[r] + state$R[i,j] * state$p[j]) / 
      (state$p[r] + state$p[j])
  }) %>% 
    unlist()

  # Create new correlation matrix
  R_new = state$R[-j, -j]
  R_new[,r - 1] <- R_new[r - 1,] <- c(rho_new_r, 1)
  
  # Update Cholesky factor
  L_new <- chol(R_new)
  
  # Update scale parameters
  sigma_new <- state$sigma[-j]
  sigma_new[r] <- sqrt(
    (state$p[r]^2 * state$sigma[r]^2 + 
       state$p[j]^2 * state$sigma[j]^2) / 
      (state$p[r] + state$p[j])^2
  )
  
  list(R=R_new, L=L_new, sigma=sigma_new, p=p_new)
}

# Simple testing code
K <- 4  # Number of parties (plus residual)
cat("\nTest 1: Initial Structure\n")
cat("------------------------\n")
state <- create_initial_structure(K)
cat("Initial vote shares (should sum to 1):", sum(state$p), "\n")
cat("Initial correlation matrix has unit diagonal:", 
    all(abs(diag(state$R) - 1) < 1e-10), "\n")
cat("Initial correlations in [-1,1]:", 
    all(abs(state$R) <= 1 + 1e-10), "\n")

cat("\nTest 2: Party Dropout\n")
cat("-------------------\n")
j <- 2  # drop second party
new_state <- handle_party_dropout(state, j)
cat("New vote shares (should sum to 1):", sum(new_state$p), "\n")
cat("New correlation matrix has unit diagonal:", 
    all(abs(diag(new_state$R) - 1) < 1e-10), "\n")
cat("New correlations in [-1,1]:", 
    all(abs(new_state$R) <= 1 + 1e-10), "\n")

cat("\nTest 3: Residual Absorption\n")
cat("-------------------------\n")
cat("Original residual + dropped party:", 
    state$p[K+1] + state$p[j], "\n")
cat("New residual:", new_state$p[K], "\n")
cat("Difference (should be near 0):", 
    abs((state$p[K+1] + state$p[j]) - new_state$p[K]), "\n")

cat("\nState Details:\n")
cat("-------------\n")
cat("\nInitial correlation matrix:\n")
print(round(state$R, 3))
cat("\nInitial vote shares:\n")
print(round(state$p, 3))
cat("\nNew correlation matrix:\n")
print(round(new_state$R, 3))
cat("\nNew vote shares:\n")
print(round(new_state$p, 3))