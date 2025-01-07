# First, let's write the Stan model
write_stan_model <- function(filename = "estimation/testing/bounded_random_walk/bounded_walk.stan") {
  stan_code <- "
  data {
    int<lower=0> N;          // number of observations
    vector[N] x;             // input locations
    vector[N] y;             // observations
  }

  parameters {
    real z_init;  // initial state (constrained)
    vector[N-1] z_delta;  // increments (constrained)
    real<lower=0> sigma;     // observation noise
    real<lower=0,upper=2> tau;       // random walk scale (constrained)
    real<lower=0,upper=0.4> a;     // lower bound in [0,1]
    real<lower=0,upper=0.5> range;  // range (ensures b <= 1)
  }

  transformed parameters {
    vector[N] z;            // latent random walk
    vector[N] mu;           // bounded random walk
    real b = a + range;     // upper bound computed to ensure b <= 1
    
    // Construct random walk
    z[1] = z_init;
    for (n in 2:N) {
      z[n] = z[n-1] + tau * z_delta[n-1];
    }
    
    // Transform to bounded walk in [a,b]
    for (n in 1:N) {
      mu[n] = a + range * inv_logit(z[n]);
    }
  }

  model {
    // Priors
    z_init ~ std_normal();
    z_delta ~ std_normal();  // standardized increments
    a ~ beta(2, 5);         // prior favoring lower values
    range ~ beta(2, 2);     // symmetric prior on range
    tau ~ normal(0, 0.5);
    sigma ~ normal(0, 0.1);
    
    // Likelihood
    y ~ normal(mu, sigma);
  }

  generated quantities {
    real upper_bound = a + range;  // actual upper bound achieved
    vector[N] y_pred;
    for (n in 1:N) {
      y_pred[n] = normal_rng(mu[n], sigma);
    }
  }
  "
  writeLines(stan_code, filename)
  return(filename)
}

# Function to generate synthetic test data
generate_test_data <- function(N = 100, seed = 42) {
  set.seed(seed)
  x <- seq(0, 10, length.out = N)
  
  # Generate bounded random walk
  z <- cumsum(c(0, rnorm(N-1, 0, 0.2)))  # reduced scale for stability
  true_a <- 0.2      # true lower bound
  true_range <- 0.3  # true range
  true_mean <- true_a + true_range * plogis(z)
  
  # Add noise
  true_sigma <- 0.02  # reduced noise to match scale
  y <- rnorm(N, true_mean, true_sigma)
  
  list(
    x = x,
    y = y,
    true_mean = true_mean,
    true_params = list(
      a = true_a,
      range = true_range,
      upper_bound = true_a + true_range,
      sigma = true_sigma
    )
  )
}

# Main script with improved sampling parameters
library(cmdstanr)
library(posterior)
library(ggplot2)
library(bayesplot)
library(tidyverse)

# Generate data
data <- generate_test_data()

# Prepare data for Stan
stan_data <- list(
  N = length(data$x),
  x = data$x,
  y = data$y
)

# Compile and fit model with improved sampling parameters
model_file <- write_stan_model()
mod <- cmdstan_model(model_file)

fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.9,
  max_treedepth = 10
)

# Check diagnostics
print("Diagnostic Summary:")
fit$diagnostic_summary()

# Extract draws
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
  # Add horizontal lines for bounds
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray50") +
  coord_cartesian(ylim = c(0, 1)) +  # force y-axis to show [0,1]
  labs(title = "Unit Interval Bounded Random Walk",
       subtitle = "Bounds shown as dashed gray lines",
       x = "x", y = "y",
       color = "Mean", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

# Parameter summary
print("\nParameter Summary:")
print(fit$summary(c("a", "range", "upper_bound", "tau", "sigma")))

# Diagnostic plots
print("\nTrace Plots for Bounds:")
mcmc_trace(fit$draws(), pars = c("a", "range", "upper_bound"))