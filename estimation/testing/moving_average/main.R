library(cmdstanr)
library(tidyverse)
library(posterior)
library(bayesplot)

# Set random seed for reproducibility
set.seed(123)

# Simulation parameters
N <- 500     # number of observations
q <- 10       # MA order
mu <- 1.5    # true mean
sigma <- 0.05  # true innovation std dev
theta <- seq(10, 1)/sum(seq(10, 1))  # true MA coefficients

# Simulate true process
epsilon <- rnorm(N, 0, sigma)
y_true <- rep(mu, N)
for(t in (q+1):N) {
  for(j in 1:q) {
    y_true[t] <- y_true[t] + theta[j] * epsilon[t-j]
  }
}

# Add measurement error
se <- runif(N, 0.01, 0.02)  # random measurement standard errors
y_obs <- rnorm(N, y_true, se)  # observed values with measurement error

# Prepare data for Stan
stan_data <- list(
  N = N,
  q = q,
  y = y_obs,
  se = se
)


# Compile model
# Assuming the model is saved as 'latent_ma.stan'
mod <- cmdstan_model("estimation/testing/moving_average/moving_average.stan")

fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,        # increased warmup
  iter_sampling = 2000,      # increased sampling
  adapt_delta = 0.9,        # increased adaptation
  max_treedepth = 10        # increased tree depth
)

# Basic diagnostics
fit$diagnostic_summary()
fit$summary()

# Extract posterior draws
draws_df <- fit$draws(format = "df")

# Plot true vs estimated parameters
true_params <- c(mu, sigma, theta)
param_names <- c("mu", "sigma", paste0("theta[", 1:q, "]"))

mcmc_hist(draws_df, pars = param_names) +
  geom_vline(data = data.frame(
    parameter = param_names,
    value = true_params
  ), aes(xintercept = value), color = "red")

# Compare true vs estimated latent states
y_true_est <- fit$summary("y_true")
plot_data <- data.frame(
  time = 1:N,
  y_obs = y_obs,
  y_true = y_true,
  y_true_est = y_true_est$median,
  y_true_lower = y_true_est$q5,
  y_true_upper = y_true_est$q95
)

ggplot(plot_data, aes(x = time)) +
  geom_ribbon(aes(ymin = y_true_lower, ymax = y_true_upper), 
              alpha = 0.2) +
  geom_line(aes(y = y_true, color = "True")) +
  geom_line(aes(y = y_true_est, color = "Estimated")) +
  geom_point(aes(y = y_obs, color = "Observed")) +
  scale_color_manual(values = c("True" = "blue", 
                                "Estimated" = "red",
                                "Observed" = "black")) +
  labs(title = "True vs Estimated Latent States",
       y = "Value",
       color = "Type")

# Examine sampling efficiency
np <- nuts_params(fit$draws())
mcmc_nuts_energy(np) +
  labs(title = "NUTS Energy Diagnostic")

