# Write the Stan model to a file
stan_code <- "
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // predictor
  vector[N] y;           // response
  matrix[N, K] B;        // B-spline basis matrix
}

parameters {
  vector[K] beta;        // spline coefficients
  real<lower=0> sigma;   // observation noise
}

transformed parameters {
  vector[N] mu = B * beta;  // fitted values
}

model {
  // Priors
  beta ~ normal(0, 5);
  sigma ~ normal(0, 1);
  
  // Likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[N] y_rep;
  
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}
"

# Write the model to a file
writeLines(stan_code, "estimation/testing/splines/unconstrained_spline.stan")

# Generate test data
set.seed(123)
N <- 1000
x <- sort(runif(N, 0, 1000))
true_f <- function(x) 2 * exp(-x/100)  # decreasing exponential function
y <- true_f(x) + rnorm(N, 0, 0.1)

# Create B-spline basis
K <- 5
B <- bs(x, df = K, degree = 3, intercept = TRUE)

# Prepare data for Stan
stan_data <- list(
  N = N,
  K = K,
  x = x,
  y = y,
  B = B
)

# Compile and fit the model
mod <- cmdstan_model("estimation/testing/splines/unconstrained_spline.stan")
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Extract posterior samples
draws <- fit$draws(c("beta", "mu"), format = "matrix")

# Calculate posterior means and intervals for the fitted values
mu_mean <- apply(draws[, grep("mu\\[", colnames(draws))], 2, mean)
mu_lower <- apply(draws[, grep("mu\\[", colnames(draws))], 2, quantile, 0.025)
mu_upper <- apply(draws[, grep("mu\\[", colnames(draws))], 2, quantile, 0.975)

# Create a plot
results_df <- data.frame(
  x = x,
  y = y,
  fit = mu_mean,
  lower = mu_lower,
  upper = mu_upper,
  true = true_f(x)
)

ggplot(results_df, aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.5) +
  geom_line(aes(y = fit), color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = true), color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Unconstrained Spline Fit",
       x = "x",
       y = "y")