# Stan model with second difference smoothing
stan_code <- "
data {
  int<lower=1> N;        // number of observations
  int<lower=1> K;        // number of basis functions
  vector[N] x;           // time
  vector[N] y;           // vote share
  matrix[N, K] B;        // B-spline basis matrix
  real<lower=0> tau;     // smoothing parameter
}

parameters {
  vector[K] beta;        // spline coefficients
  real<lower=0> sigma;   // observation noise
}

transformed parameters {
  vector[N] mu = B * beta;
}

model {
  // Likelihood
  y ~ normal(mu, sigma);
  
  // Prior on noise
  sigma ~ normal(0, 1);
  
  // Second differences smoothing prior
  for (k in 3:K) {
    (beta[k] - 2 * beta[k-1] + beta[k-2]) ~ normal(0, tau);
  }
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}
"

writeLines(stan_code, "second_diff_spline.stan")

# Generate synthetic data that mimics party support patterns
set.seed(123)
N <- 200  # quarterly data over 50 years
x <- seq(1970, 2020, length.out = N)

# True long-term decline with different components
true_trend <- function(x) {
  # Baseline support starting at 45%
  base <- 45
  
  # Long-term decline (-0.3% per year)
  decline <- (x - 1970) * -0.3
  
  # Add slight non-linearity to make it interesting
  nonlinear <- 0.2 * ((x - 1995)/10)^2
  
  return(base + decline + nonlinear)
}

# Generate data with realistic noise
y <- true_trend(x) + rnorm(N, 0, 1.5)

# Test different smoothing parameters
tau_values <- c(1, 0.1, 0.01)  # from weak to strong smoothing
K <- 5  # moderate number of knots
fits <- list()

for(tau in tau_values) {
  # Create B-spline basis
  B <- bs(x, df = K, degree = 3, intercept = TRUE)
  
  # Prepare data for Stan
  stan_data <- list(
    N = N,
    K = K,
    x = x,
    y = y,
    B = B,
    tau = tau
  )
  
  # Compile and fit model
  mod <- cmdstan_model("second_diff_spline.stan")
  fit <- mod$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000
  )
  
  # Extract posterior samples
  draws <- fit$draws(c("mu", "sigma"), format = "matrix")
  
  # Calculate posterior means and intervals
  mu_mean <- apply(draws[, 1:N], 2, mean)
  mu_lower <- apply(draws[, 1:N], 2, quantile, 0.025)
  mu_upper <- apply(draws[, 1:N], 2, quantile, 0.975)
  sigma_mean <- mean(draws[, N + 1])
  
  # Store results
  fits[[length(fits) + 1]] <- data.frame(
    x = x,
    y = y,
    fit = mu_mean,
    lower = mu_lower,
    upper = mu_upper,
    true = true_trend(x),
    tau = tau,
    sigma = sigma_mean
  )
}

# Combine results
results_df <- do.call(rbind, fits)
results_df$tau <- factor(results_df$tau, 
                         levels = tau_values,
                         labels = paste("tau =", tau_values))

# Create comparison plot
p1 <- ggplot(results_df, aes(x = x)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = fit), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = true), color = "red", linetype = "dashed") +
  facet_wrap(~tau) +
  theme_minimal() +
  labs(title = "Long-term Trend with Second Difference Smoothing",
       subtitle = "Red dashed = true trend, Lower tau = stronger smoothing",
       x = "Year",
       y = "Vote Share (%)")

# Calculate and plot first derivatives
dx <- diff(x)[1]  # assuming equal spacing
derivatives_df <- do.call(rbind, lapply(fits, function(df) {
  data.frame(
    x = x[-1],
    derivative = diff(df$fit)/dx,
    tau = df$tau[1]
  )
}))

p2 <- ggplot(derivatives_df, aes(x = x)) +
  geom_line(aes(y = derivative), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~tau) +
  theme_minimal() +
  labs(title = "First Derivatives of Fitted Trends",
       x = "Year",
       y = "Rate of Change (% per year)")

# Print diagnostic information
for(fit_data in fits) {
  cat(sprintf("\nResults for tau = %s:\n", unique(fit_data$tau)))
  cat(sprintf("Average sigma: %.3f\n", unique(fit_data$sigma)))
  cat(sprintf("Average absolute second derivative: %.3f\n", 
              mean(abs(diff(diff(fit_data$fit))/dx^2))))
}

# Print plots side by side if in notebook, or one after another if in console
print(p1)
print(p2)