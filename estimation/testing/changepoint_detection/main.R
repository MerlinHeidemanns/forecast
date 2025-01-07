library(cmdstanr)
library(tidyverse)
library(bayesplot)


# Compile the model
mod <- cmdstan_model("estimation/testing/changepoint_detection/model.stan")

# Generate some example data
set.seed(123)
T <- 50  # number of time points
P <- 5   # number of pollsters
N <- 200 # number of polls

# True parameters
true_sigma <- 0.1  # innovation SD on logit scale
true_tau <- 0.2    # pollster effect SD on logit scale
true_eta <- 0.1    # scale for methodology changes

# Generate true latent state (random walk on logit scale)
true_theta_logit <- cumsum(c(0, rnorm(T-1, 0, true_sigma)))
true_theta_prob <- plogis(true_theta_logit)

# Generate pollster biases (on logit scale)
true_alpha <- rnorm(P, 0, true_tau)

# Generate sparse methodology changes for pollster 1
true_delta <- rep(0, T)
# Simulate one major methodology change
changepoint <- 25
true_delta[changepoint:T] <- rcauchy(1, 0, true_eta)

# Generate observed data
time <- sample(1:T, N, replace = TRUE)
pollster <- sample(1:P, N, replace = TRUE)
n_resp <- rpois(N, lambda = 1000)

# Calculate true probabilities including methodology changes
true_p <- plogis(true_theta_logit[time] + true_alpha[pollster] + 
                   ifelse(pollster == 1, true_delta[time], 0))
n_pos <- rbinom(N, n_resp, true_p)

# Prepare data for Stan
stan_data <- list(
  N = N,
  T = T,
  P = P,
  n_resp = n_resp,
  n_pos = n_pos,
  time = time,
  pollster = pollster,
  sigma_prior = 0.5,
  tau_prior = 0.5,
  eta_prior = 0.5
)

# Fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)

# Basic diagnostics
fit$summary(c("sigma", "tau", "eta"))
mcmc_trace(fit$draws(c("sigma", "tau", "eta")))

# Extract posterior draws using posterior package
draws_delta <- as_draws_matrix(fit$draws("delta"))
draws_theta_prob <- as_draws_matrix(fit$draws("theta_prob"))
draws_change_prob <- as_draws_matrix(fit$draws("change_prob"))

# Calculate posterior summaries
delta_mean <- colMeans(draws_delta)
delta_lower <- apply(draws_delta, 2, quantile, 0.025)
delta_upper <- apply(draws_delta, 2, quantile, 0.975)
change_prob_mean <- colMeans(draws_change_prob)

theta_prob_mean <- colMeans(draws_theta_prob)
theta_prob_lower <- apply(draws_theta_prob, 2, quantile, 0.025)
theta_prob_upper <- apply(draws_theta_prob, 2, quantile, 0.975)

# Plot methodology changes
change_df <- data.frame(
  time = 1:T,
  true_delta = true_delta,
  est_delta = delta_mean,
  lower = delta_lower,
  upper = delta_upper,
  change_prob = change_prob_mean
)

# Plot estimated vs true methodology changes
p1 <- ggplot(change_df, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = true_delta, color = "True")) +
  geom_line(aes(y = est_delta, color = "Estimated")) +
  labs(title = "Pollster 1 Methodology Changes",
       y = "Effect Size (Logit Scale)",
       color = "Series") +
  theme_minimal()

# Plot probability of methodology change
p2 <- ggplot(change_df, aes(x = time, y = change_prob)) +
  geom_line() +
  geom_vline(xintercept = changepoint, linetype = "dashed", color = "red") +
  labs(title = "Probability of Methodology Change",
       y = "Probability") +
  theme_minimal()

# Plot results with poll results
plot_df <- data.frame(
  time = 1:T,
  true = true_theta_prob,
  est = theta_prob_mean,
  lower = theta_prob_lower,
  upper = theta_prob_upper
)

obs_df <- data.frame(
  time = time,
  prop = n_pos / n_resp,
  pollster = factor(pollster)
)

p3 <- ggplot(plot_df, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = true, color = "True")) +
  geom_line(aes(y = est, color = "Estimated")) +
  geom_point(data = obs_df, aes(y = prop, color = pollster), alpha = 0.5) +
  geom_vline(xintercept = changepoint, linetype = "dashed", color = "red") +
  labs(title = "Support Over Time with Poll Results",
       y = "Support Probability",
       color = "Source") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

# Posterior predictive checks
draws_y_rep <- as_draws_matrix(fit$draws("y_rep"))
ppc_dens_overlay(n_pos / n_resp, 
                 draws_y_rep[1:50,] / rep(n_resp, each = 50))

# Save plots
plots <- list(
  methodology_changes = p1,
  change_probability = p2,
  support_trend = p3
)

# Print diagnostics
fit$diagnostic_summary()

# Print model summary
model_summary <- fit$summary()
head(model_summary, n = 20)  # View first 20 parameters

# Arrange plots
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)


