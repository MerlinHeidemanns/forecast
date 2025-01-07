library(cmdstanr)
library(tidyverse)

# Generate synthetic data
set.seed(123)
N_fixed <- 10
N_obs <- 40
noise_scale <- 0.1

# Create input locations
x_fixed <- seq(1, 10, length.out = N_fixed)
x_obs <- seq(11, 50, length.out = N_obs)

# True function (for simulation)
true_f <- function(x) sin(x/5) + 0.2 * cos(x/2)

# Generate data
y_fixed <- true_f(x_fixed)
y_obs <- true_f(x_obs) + rnorm(N_obs, 0, noise_scale)

# Prepare data for Stan
data_list <- list(
  N_fixed = N_fixed,
  N_obs = N_obs,
  N = N_fixed + N_obs,
  x_fixed = x_fixed,
  y_fixed = y_fixed,
  x_obs = x_obs,
  y_obs = y_obs,
  noise_scale = noise_scale
)

# Compile and fit model
mod <- cmdstan_model("estimation/testing/conditional_gp/gp_model_conditional.stan")
fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Extract predictions and plot
predictions <- fit$draws("f_pred", format = "matrix")
x_pred <- seq(1, 50, length.out = 200)

# Calculate prediction mean and credible intervals
pred_mean <- colMeans(predictions)
pred_lower <- apply(predictions, 2, quantile, probs = 0.025)
pred_upper <- apply(predictions, 2, quantile, probs = 0.975)

# Create plotting data
plot_data <- tibble(
  x = x_pred,
  mean = pred_mean,
  lower = pred_lower,
  upper = pred_upper
)
# Compile model

# Fit model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 100
)

# Print diagnostics
fit$diagnostic_summary()

# Extract predictions and function values
draws_df <- fit$draws(variables = c("y_pred", "f_all"), format = "df") %>%
  dplyr::select(!contains("."))
predictions <- draws_df %>%
  dplyr::select(starts_with("y_pred"))
f_all <- draws_df %>%
  dplyr::select(starts_with("f_all"))

# Calculate posterior mean and credible intervals for both predictions and function values
pred_mean <- apply(predictions, 2, mean)
pred_lower <- apply(predictions, 2, quantile, probs = 0.025)
pred_upper <- apply(predictions, 2, quantile, probs = 0.975)

f_mean <- apply(f_all, 2, mean)
f_lower <- apply(f_all, 2, quantile, probs = 0.025)
f_upper <- apply(f_all, 2, quantile, probs = 0.975)

# Create plotting data
plot_data <- data.frame(
  x = data$x_grid,
  f_mean = f_mean,
  f_lower = f_lower,
  f_upper = f_upper
)

obs_data <- data.frame(
  x = data$x_obs,
  y = data$y_obs
)

fixed_data <- data.frame(
  x = data$x_fixed,
  y = data$y_fixed
)

# Add true function line
x_curve <- seq(1, 50, length.out = 200)
true_curve <- data.frame(
  x = x_curve,
  y = data$true_fn(x_curve)
)

# Create plot
ggplot() +
  geom_line(data = true_curve,
            aes(x = x, y = y),
            color = "gray50",
            linetype = "dashed") +
  geom_ribbon(data = plot_data,
              aes(x = x, ymin = f_lower, ymax = f_upper),
              alpha = 0.2) +
  geom_line(data = plot_data,
            aes(x = x, y = f_mean),
            color = "blue") +
  geom_point(data = obs_data,
             aes(x = x, y = y),
             color = "black") +
  geom_point(data = fixed_data,
             aes(x = x, y = y),
             color = "red",
             size = 3) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  labs(title = "Gaussian Process with Fixed Points",
       subtitle = "Domain: [1, 50] with regularly spaced points",
       x = "x",
       y = "y",
       caption = "Black points: observations, Red points: fixed values\nBlue line: posterior mean, Gray band: 95% credible interval\nDashed line: true function")



library(cmdstanr)
library(ggplot2)
library(posterior)

# Set random seed for reproducibility
set.seed(123)

# Generate synthetic data
generate_data <- function(N_obs = 15, N_fixed = 5, noise_sd = 0.1) {
  # Generate regular grid from 1 to 50
  x_grid <- seq(1, 50, by = 1)
  N_total <- length(x_grid)
  
  # Randomly select points for observations and fixed values
  n_points_needed <- N_obs + N_fixed
  selected_indices <- sample(N_total, n_points_needed, replace = FALSE)
  obs_indices <- selected_indices[1:N_obs]
  fixed_indices <- selected_indices[(N_obs + 1):n_points_needed]
  
  x_obs <- x_grid[obs_indices]
  x_fixed <- x_grid[fixed_indices]
  
  # True function (sum of two sinusoids)
  true_fn <- function(x) sin(x/5) + 0.5*sin(x/2)
  
  # Generate observations with noise
  y_obs <- true_fn(x_obs) + rnorm(N_obs, 0, noise_sd)
  
  # Generate fixed values (no noise)
  y_fixed <- true_fn(x_fixed)
  
  list(
    x_obs = x_obs,
    y_obs = y_obs,
    x_fixed = x_fixed,
    y_fixed = y_fixed,
    true_fn = true_fn,
    x_grid = x_grid
  )
}

# Generate data
data <- generate_data()

# Prepare data for Stan
stan_data <- list(
  N = length(data$x_grid),
  N_obs = length(data$x_obs),
  N_fixed = length(data$x_fixed),
  x_grid = data$x_grid,
  obs_idx = match(data$x_obs, data$x_grid),
  fixed_idx = match(data$x_fixed, data$x_grid),
  y_obs = data$y_obs,
  y_fixed = data$y_fixed,
  sigma = 0.1,
  alpha = 1.0,
  rho = 5.0,  # Increased length scale for the wider domain
  nugget = 1e-6
)

# Compile model
mod <- cmdstan_model("estimation/testing/conditional_gp/gp_model_conditional.stan")

# Fit model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 500
)

# Print diagnostics
fit$diagnostic_summary()

# Extract predictions
draws_df <- fit$draws(variables = "f", format = "df") %>%
  dplyr::select(!contains("."))

# Calculate posterior mean and credible intervals
pred_mean <- apply(draws_df, 2, mean)
pred_lower <- apply(draws_df, 2, quantile, probs = 0.025)
pred_upper <- apply(draws_df, 2, quantile, probs = 0.975)

# Create plotting data
plot_data <- data.frame(
  x = data$x_obs,
  y = data$y_obs,
  pred_mean = pred_mean,
  pred_lower = pred_lower,
  pred_upper = pred_upper
)

fixed_data <- data.frame(
  x = data$x_fixed,
  y = data$y_fixed
)

# Add true function line
x_curve <- seq(1, 50, length.out = 200)
true_curve <- data.frame(
  x = x_curve,
  y = data$true_fn(x_curve)
)

# Create plot
ggplot() +
  geom_line(data = true_curve,
            aes(x = x, y = y),
            color = "gray50",
            linetype = "dashed") +
  geom_ribbon(data = plot_data,
              aes(x = x, ymin = pred_lower, ymax = pred_upper),
              alpha = 0.2) +
  geom_line(data = plot_data,
            aes(x = x, y = pred_mean),
            color = "blue") +
  geom_point(data = plot_data,
             aes(x = x, y = y),
             color = "black") +
  geom_point(data = fixed_data,
             aes(x = x, y = y),
             color = "red",
             size = 3) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  labs(title = "Gaussian Process with Fixed Points",
       subtitle = "Domain: [1, 50] with regularly spaced points",
       x = "x",
       y = "y",
       caption = "Black points: observations, Red points: fixed values\nBlue line: posterior mean, Gray band: 95% credible interval\nDashed line: true function")

