library(cmdstanr)
library(tidyverse)
library(MASS)
library(posterior)
set.seed(123)

# Generate synthetic data
generate_gp_data <- function(N = 50, N_fixed = 10, alpha = 1, rho = 5, sigma = 0.1) {
  # Generate regularly spaced locations
  x <- seq(1, 50, length.out = N)
  
  # Randomly select points to be fixed
  fixed_idx <- sort(sample(N, N_fixed))
  obs_idx <- sort(setdiff(1:N, fixed_idx))
  N_obs <- N - N_fixed
  
  # Compute covariance matrix
  K <- matrix(0, N, N)
  for(i in 1:N) {
    for(j in 1:N) {
      dist <- abs(x[i] - x[j])
      K[i,j] <- alpha^2 * exp(-dist^2 / (2 * rho^2))
    }
  }
  
  # Generate true function values
  f_true <- mvrnorm(1, rep(0, N), K)
  
  # Split into observed and fixed
  y_obs <- f_true[obs_idx] + rnorm(N_obs, 0, sigma)
  y_fixed <- f_true[fixed_idx]
  
  return(list(
    x = x,
    obs_idx = obs_idx,
    fixed_idx = fixed_idx,
    y_obs = y_obs,
    y_fixed = y_fixed,
    f_true = f_true
  ))
}

# Generate data
data <- generate_gp_data()

# Prepare Stan data
stan_data <- list(
  N = length(data$x),
  N_obs = length(data$obs_idx),
  N_fixed = length(data$fixed_idx),
  x = data$x,
  obs_idx = data$obs_idx,
  fixed_idx = data$fixed_idx,
  y_obs = data$y_obs,
  y_fixed = data$y_fixed,
  sigma = 0.1
)

# Compile and fit model
mod <- cmdstan_model("estimation/testing/conditional_gp/gp_model.stan")

# Fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.9
)

# Basic diagnostics
print(fit$summary(c("alpha", "rho")))
print(fit$diagnostic_summary())

# Extract posterior draws
draws <- fit$draws("f")
f_post_mean <- colMeans(as_draws_matrix(draws))
f_post_lower <- apply(as_draws_matrix(draws), 2, quantile, probs = 0.025)
f_post_upper <- apply(as_draws_matrix(draws), 2, quantile, probs = 0.975)

# Create plotting data
plot_data <- tibble(
  x = data$x,
  f_true = data$f_true,
  f_post = f_post_mean,
  type = "fixed"
) %>%
  mutate(
    type = case_when(
      row_number() %in% data$obs_idx ~ "observed",
      row_number() %in% data$fixed_idx ~ "fixed",
      TRUE ~ "unobserved"
    )
  )
plot_data <- tibble(
  x = data$x,
  f_true = data$f_true,
  f_post = f_post_mean,
  f_lower = f_post_lower,
  f_upper = f_post_upper,
  type = case_when(
    seq_along(data$x) %in% data$obs_idx ~ "observed",
    seq_along(data$x) %in% data$fixed_idx ~ "fixed",
    TRUE ~ "unobserved"
  )
)

# Create plot with uncertainty intervals
p <- ggplot(plot_data, aes(x = x)) +
  # Add uncertainty ribbon
  geom_ribbon(aes(ymin = f_lower, ymax = f_upper),
              fill = "gray80", alpha = 0.5) +
  # Add true function
  geom_line(aes(y = f_true), linetype = "dashed", color = "blue", alpha = 0.5) +
  # Add posterior mean
  geom_line(aes(y = f_post), color = "red", alpha = 0.7) +
  # Add points
  geom_point(data = subset(plot_data, type == "observed"),
             aes(y = f_true), color = "blue", size = 2) +
  geom_point(data = subset(plot_data, type == "fixed"),
             aes(y = f_true), color = "darkblue", shape = 17, size = 3) +
  # Styling
  theme_minimal() +
  labs(title = "GP Posterior Mean with 95% Credible Intervals",
       subtitle = "Triangles = Fixed Points, Circles = Noisy Observations",
       x = "Location", 
       y = "f(x)") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )




# Compile and fit model
mod <- cmdstan_model("estimation/testing/conditional_gp/gp_model.stan")

# Fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.9
)

# Basic diagnostics
print(fit$summary(c("alpha", "rho")))
print(fit$diagnostic_summary())

# Extract posterior draws
draws <- fit$draws()
f_post_mean <- posterior::summarise_draws(draws, ~mean(.x))$mean

# Create plotting data
plot_data <- tibble(
  x = c(stan_data$x_obs, stan_data$x_fixed),
  f_true = data$f_true,
  f_post = f_post_mean[grep("^f\\[", rownames(f_post_mean))],
  type = c(rep("observed", stan_data$N_obs), 
           rep("fixed", stan_data$N_fixed))
)

# Plot true vs posterior mean with better formatting
p <- ggplot(plot_data, aes(x = x)) +
  geom_line(aes(y = f_true), linetype = "dashed", color = "blue", alpha = 0.5) +
  geom_line(aes(y = f_post), color = "red", alpha = 0.7) +
  geom_point(data = subset(plot_data, type == "observed"),
             aes(y = f_true), color = "blue", size = 2) +
  geom_point(data = subset(plot_data, type == "fixed"),
             aes(y = f_true), color = "darkblue", shape = 17, size = 3) +
  theme_minimal() +
  labs(title = "GP Posterior Mean vs True Function",
       subtitle = "Triangles = Fixed Points, Circles = Noisy Observations",
       x = "Location", 
       y = "f(x)") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Print plot
print(p)

# Check if fixed points are recovered
fixed_idx <- (stan_data$N_obs + 1):length(f_post_mean[grep("^f\\[", rownames(f_post_mean))])
fixed_errors <- abs(f_post_mean[grep("^f\\[", rownames(f_post_mean))][fixed_idx] - 
                      stan_data$y_fixed)
cat("\nMaximum error at fixed points:", max(fixed_errors), "\n")

# Calculate and print R-squared for prediction
pred_r2 <- 1 - sum((data$f_true - f_post_mean[grep("^f\\[", rownames(f_post_mean))])^2) / 
  sum((data$f_true - mean(data$f_true))^2)
cat("R-squared for prediction:", pred_r2, "\n\n")

# Print MCMC efficiency statistics for key parameters
cat("MCMC diagnostics for key parameters:\n")
posterior::summarise_draws(
  draws[, c("alpha", "rho")],
  ~c(rhat = posterior::rhat(.x),
     ess_bulk = posterior::ess_bulk(.x),
     ess_tail = posterior::ess_tail(.x))
) %>% print()






library(cmdstanr)
library(tidyverse)
library(MASS)  # for mvrnorm
library(posterior)  # for working with draws
set.seed(123)

# Generate synthetic data
generate_gp_data <- function(N_obs = 20, N_fixed = 5, alpha = 1, rho = 0.5, sigma = 0.1) {
  # Generate input locations
  x_obs <- matrix(runif(N_obs), ncol = 1)  # 1D for simplicity
  x_fixed <- matrix(seq(0, 1, length.out = N_fixed), ncol = 1)
  
  # Compute full covariance matrix
  x_all <- rbind(x_obs, x_fixed)
  N_total <- N_obs + N_fixed
  K <- matrix(0, N_total, N_total)
  
  for(i in 1:N_total) {
    for(j in 1:N_total) {
      dist <- abs(x_all[i] - x_all[j])
      K[i,j] <- alpha^2 * exp(-dist^2 / (2 * rho^2))
    }
  }
  
  # Generate true function values
  f_true <- mvrnorm(1, rep(0, N_total), K)
  
  # Add noise to observations
  y_obs <- f_true[1:N_obs] + rnorm(N_obs, 0, sigma)
  y_fixed <- f_true[(N_obs+1):N_total]
  
  return(list(
    x_obs = x_obs,
    y_obs = y_obs,
    x_fixed = x_fixed,
    y_fixed = y_fixed,
    f_true = f_true
  ))
}

# Generate data
data <- generate_gp_data()

# Prepare Stan data
stan_data <- list(
  N_obs = length(data$y_obs),
  N_fixed = length(data$y_fixed),
  D = 1,
  x_obs = data$x_obs,
  x_fixed = data$x_fixed,
  y_obs = data$y_obs,
  y_fixed = data$y_fixed,
  sigma = 0.1
)

# Compile and fit model
# Note: Save the Stan model as 'gp_model.stan'
mod <- cmdstan_model("estimation/testing/conditional_gp/gp_model.stan")

# Fit the model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.9
)

# Basic diagnostics
fit$summary(c("alpha", "rho"))
fit$diagnostic_summary()

# Extract posterior draws as a draws_matrix object
draws <- fit$draws("f")
f_post_mean <- posterior::summarise_draws(draws)$mean

# Create plotting data
plot_data <- tibble(
  x = c(stan_data$x_obs, stan_data$x_fixed),
  f_true = data$f_true,
  f_post = f_post_mean,
  type = c(rep("observed", stan_data$N_obs), 
           rep("fixed", stan_data$N_fixed))
)

# Plot true vs posterior mean
p <- ggplot(plot_data, aes(x = x)) +
  geom_line(aes(y = f_true), linetype = "dashed", color = "blue") +
  geom_line(aes(y = f_post), color = "red") +
  geom_point(data = subset(plot_data, type == "observed"),
             aes(y = f_true), color = "blue") +
  geom_point(data = subset(plot_data, type == "fixed"),
             aes(y = f_true), color = "blue", shape = 17, size = 3) +
  theme_minimal() +
  labs(title = "GP Posterior Mean vs True Function",
       x = "x", y = "f(x)")

# Print plot
print(p)

# Check if fixed points are recovered
fixed_idx <- (stan_data$N_obs + 1):length(f_post_mean[grep("^f\\[", rownames(f_post_mean))])
fixed_errors <- abs(f_post_mean[grep("^f\\[", rownames(f_post_mean))][fixed_idx] - 
                      stan_data$y_fixed)
cat("Maximum error at fixed points:", max(fixed_errors), "\n")

# Calculate and print R-squared for prediction
pred_r2 <- 1 - sum((data$f_true - f_post_mean[grep("^f\\[", rownames(f_post_mean))])^2) / 
  sum((data$f_true - mean(data$f_true))^2)
cat("R-squared for prediction:", pred_r2, "\n")

# Additional diagnostics available in cmdstanr
fit$diagnostic_summary()
fit$sampler_diagnostics()

# Print MCMC efficiency statistics
posterior::summarise_draws(
  draws,
  ~c(rhat = posterior::rhat(.x),
     ess_bulk = posterior::ess_bulk(.x),
     ess_tail = posterior::ess_tail(.x))
)