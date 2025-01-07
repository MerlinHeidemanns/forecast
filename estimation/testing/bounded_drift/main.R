## We can model a bounded drift.




# First, let's create a function to write the Stan model
write_stan_model <- function(filename = "estimation/testing/bounded_drift/drift_model.stan") {
  stan_code <- "
  data {
    int<lower=0> N;        // number of observations
    vector[N] x;           // input locations
    vector[N] y;           // observations
  }

  parameters {
    real mu_min;           // minimum asymptote
    real<lower=0> mu_range;// range (mu_max - mu_min)
    real<lower=0> r;       // rate of transition
    real c;                // center point
    real<lower=0> sigma;   // observation noise
  }

  model {
    // Priors
    mu_min ~ normal(0, 10);
    mu_range ~ normal(5, 5);
    r ~ normal(1, 1);
    c ~ normal(5, 5);
    sigma ~ normal(0, 1);
    
    // Likelihood
    for (n in 1:N) {
      real mu = mu_min + mu_range * inv_logit(r * (x[n] - c));
      y[n] ~ normal(mu, sigma);
    }
  }

  generated quantities {
    real mu_max = mu_min + mu_range;
    vector[N] y_pred;
    for (n in 1:N) {
      real mu = mu_min + mu_range * inv_logit(r * (x[n] - c));
      y_pred[n] = normal_rng(mu, sigma);
    }
  }
  "
  writeLines(stan_code, filename)
  return(filename)
}

# Function to generate synthetic data
generate_test_data <- function(N = 100, seed = 42) {
  set.seed(seed)
  x <- seq(0, 10, length.out = N)
  
  # True parameters
  true_params <- list(
    mu_min = 2,
    mu_max = 8,  # note: mu_range will be 6
    r = 0.5,
    c = 5,
    sigma = 0.5
  )
  
  # Generate true mean
  true_mean <- true_params$mu_min + 
    (true_params$mu_max - true_params$mu_min) / 
    (1 + exp(-true_params$r * (x - true_params$c)))
  
  # Generate noisy observations
  y <- rnorm(N, true_mean, true_params$sigma)
  
  list(
    x = x,
    y = y,
    true_params = true_params,
    true_mean = true_mean
  )
}

# Main script to run the analysis
library(cmdstanr)
library(posterior)
library(ggplot2)
library(bayesplot)
library(tidyverse)

# Write Stan model and compile
model_file <- write_stan_model()
mod <- cmdstan_model(model_file)

# Generate data
data <- generate_test_data()

# Prepare data for Stan
stan_data <- list(
  N = length(data$x),
  x = data$x,
  y = data$y
)

# Fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500  # Print update every 500 iterations
)

# Check diagnostics
fit$diagnostic_summary()
print(fit$summary())

# Extract draws in a tidy format
draws_df <- fit$draws(format = "df")

# Calculate posterior predictions
y_pred_cols <- grep("y_pred", colnames(draws_df), value = TRUE)
pred_intervals <- apply(draws_df[, y_pred_cols], 2, 
                        quantile, probs = c(0.025, 0.5, 0.975))

# Create plotting data
plot_data <- data.frame(
  x = data$x,
  y = data$y,
  true_mean = data$true_mean,
  fit = pred_intervals[2,],
  lower = pred_intervals[1,],
  upper = pred_intervals[3,]
)

# Create plot
p <- ggplot(plot_data, aes(x = x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% CI"), 
              alpha = 0.2) +
  geom_line(aes(y = fit, color = "Estimated"), size = 1) +
  geom_line(aes(y = true_mean, color = "True"), 
            linetype = "dashed", size = 1) +
  geom_point(aes(y = y), alpha = 0.5) +
  scale_color_manual(values = c("Estimated" = "blue", 
                                "True" = "red")) +
  scale_fill_manual(values = c("95% CI" = "blue")) +
  labs(title = "Drift Model Parameter Recovery",
       subtitle = "Points: observations, Blue: estimated, Red: true",
       x = "x", y = "y",
       color = "Mean", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

# Compare true vs estimated parameters
true_vs_est <- data.frame(
  Parameter = c("mu_min", "mu_max", "r", "c", "sigma"),
  True = c(
    data$true_params$mu_min,
    data$true_params$mu_max,
    data$true_params$r,
    data$true_params$c,
    data$true_params$sigma
  ),
  Estimated = c(
    mean(draws_df$mu_min),
    mean(draws_df$mu_max),
    mean(draws_df$r),
    mean(draws_df$c),
    mean(draws_df$sigma)
  ),
  SD = c(
    sd(draws_df$mu_min),
    sd(draws_df$mu_max),
    sd(draws_df$r),
    sd(draws_df$c),
    sd(draws_df$sigma)
  )
)

print(true_vs_est, digits = 3)

# Add some diagnostic plots
# Trace plots
mcmc_trace(fit$draws(), 
           pars = c("mu_min", "mu_range", "r", "c", "sigma"))

# Posterior distributions
mcmc_hist(fit$draws(), 
          pars = c("mu_min", "mu_range", "r", "c", "sigma"))

# Show convergence diagnostics
print("Convergence diagnostics:")
fit$diagnostic_summary()