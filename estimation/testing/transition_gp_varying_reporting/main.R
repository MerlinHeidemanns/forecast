library(cmdstanr)
library(tidyverse)

# Function to generate GP covariance matrix
gp_cov_matrix <- function(t, alpha, rho) {
  n <- length(t)
  Sigma <- matrix(0, n, n)
  for(i in 1:n) {
    for(j in 1:n) {
      Sigma[i,j] <- alpha^2 * exp(-(t[i] - t[j])^2 / (2 * rho^2))
    }
  }
  return(Sigma)
}

# Function to compute party weights
party_weight <- function(t, t_emerge, t_disappear, rate) {
  emergence <- plogis(rate * (t - t_emerge))
  survival <- plogis(-rate * (t - t_disappear))
  emergence * survival
}

# Set true parameters
set.seed(123)
true_alpha <- 0.15
true_rho <- 7.0
true_transition_rates <- c(3, 5)

# Time parameters
N <- 100  # number of observations
T <- 50   # unique time points
time_points <- seq(1, T)
K_fixed <- 4
K_trans <- 2

# Generate true correlation matrix using LKJ prior
true_L_omega <- t(chol(rWishart(1, K_fixed + K_trans - 1, diag(K_fixed + K_trans - 1))[,,1]))
true_Omega <- true_L_omega %*% t(true_L_omega)

# Generate GP for latent vote shares
Sigma <- gp_cov_matrix(time_points, true_alpha, true_rho)
L_k <- chol(Sigma + diag(1e-9, T))

# Generate latent vote shares
eta <- matrix(rnorm(T * (K_fixed + K_trans - 1)), T, K_fixed + K_trans - 1)
f <- matrix(0, T, K_fixed + K_trans)
f[,1:(K_fixed + K_trans - 1)] <- L_k %*% eta %*% true_L_omega

# Transition times
t_emerge <- c(2, 4)
t_disappear <- c(20, 50)

# Apply transition weights
for(k in 1:K_trans) {
  w <- party_weight(time_points, t_emerge[k], t_disappear[k], true_transition_rates[k])
  f[, K_fixed + k] <- w * f[, K_fixed + k] + (1 - w) * (-100)
}

# Generate observation times
time_idx <- sample(1:T, N, replace = TRUE)
n_resp <- rpois(N, lambda = 1000)  # sample sizes

# Create reporting patterns
R <- 3
pattern_matrix <- matrix(c(
  1, 1,  # pattern 1: both parties reported
  1, 0,  # pattern 2: only first transitioning party
  0, 1   # pattern 3: only second transitioning party
), nrow = R, byrow = TRUE)

# Assign and sort by reporting pattern
reporting_pattern <- sample(1:R, N, replace = TRUE)
sort_idx <- order(reporting_pattern)
reporting_pattern <- reporting_pattern[sort_idx]
time_idx <- time_idx[sort_idx]
n_resp <- n_resp[sort_idx]

# Calculate pattern starts and ends
pattern_starts <- matrix(0, nrow = R, ncol = 2)
current_pos <- 1
for(r in 1:R) {
  pattern_length <- sum(reporting_pattern == r)
  pattern_starts[r,] <- c(current_pos, current_pos + pattern_length - 1)
  current_pos <- current_pos + pattern_length
}

# Generate vote counts using the model's probability structure
y_obs <- matrix(0, nrow = K_fixed + K_trans, ncol = N)

# Calculate probabilities for each observation
ones <- matrix(1, K_fixed + K_trans, K_fixed + K_trans)
tmp <- f[time_idx,]
probs <- exp(tmp - log(exp(tmp) %*% ones))

# Generate aggregation matrices
agg_matrix <- vector("list", R)
for(r in 1:R) {
  pattern_agg <- diag(K_fixed + K_trans)
  for(k in 1:K_trans) {
    if(pattern_matrix[r,k] == 0) {
      pattern_agg[K_fixed, K_fixed + k] <- 1.0
      pattern_agg[K_fixed + k, K_fixed + k] <- 0.0
    }
  }
  agg_matrix[[r]] <- pattern_agg
}

# Calculate pattern-specific probabilities and generate votes
for(r in 1:R) {
  start <- pattern_starts[r,1]
  end <- pattern_starts[r,2]
  pattern_probs <- probs[start:end,] %*% agg_matrix[[r]]
  
  for(i in start:end) {
    y_obs[,i] <- rmultinom(1, n_resp[i], pattern_probs[i - start + 1,])
  }
}

# Create observation indices
N_obs <- rep(0, K_fixed + K_trans)
obs_idx <- matrix(0, nrow = K_fixed + K_trans, ncol = N)

for(k in 1:(K_fixed + K_trans)) {
  if(k <= K_fixed) {
    N_obs[k] <- N
    obs_idx[k,] <- 1:N
  } else {
    k_trans <- k - K_fixed
    valid_obs <- which(sapply(reporting_pattern, function(r) pattern_matrix[r, k_trans] == 1))
    N_obs[k] <- length(valid_obs)
    obs_idx[k, 1:N_obs[k]] <- valid_obs
  }
}

# Prepare data for Stan
stan_data <- list(
  N = N,
  K_fixed = K_fixed,
  K_trans = K_trans,
  T = T,
  time_points = time_points,
  time_idx = time_idx,
  n_resp = n_resp,
  N_obs = N_obs,
  y_obs = y_obs,
  obs_idx = obs_idx,
  R = R,
  pattern_starts = pattern_starts,
  reporting_pattern = reporting_pattern,
  pattern_matrix = pattern_matrix,
  t_emerge = t_emerge,
  t_disappear = t_disappear,
  residual_idx = K_fixed
)

# Input validation
stopifnot(
  all(diff(pattern_starts[,1]) > 0),
  all(pattern_starts[,2] - pattern_starts[,1] >= 0),
  max(pattern_starts) <= N,
  min(pattern_starts) >= 1,
  all(sapply(1:R, function(r) {
    all(reporting_pattern[pattern_starts[r,1]:pattern_starts[r,2]] == r)
  }))
)

# Fit mod
# Compile model
mod <- cmdstan_model("estimation/testing/transition_gp_varying_reporting/mod.stan",
                     stanc_options = list("O1"), compile = TRUE)

# Fit model
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  refresh = 100,
  adapt_delta = 0.8,
  max_treedepth = 10
)




# Extract posterior draws of shares
draws_shares <- fit$draws("shares", format="draws_df")

# Calculate posterior means and credible intervals for each party and time point
process_shares <- function(party_idx) {
  shares_party <- draws_shares[, paste0("shares[", 1:T, ",", party_idx, "]")]
  data.frame(
    time = seq(1, 50),
    mean = apply(shares_party, 2, mean),
    lower = apply(shares_party, 2, quantile, 0.025),
    upper = apply(shares_party, 2, quantile, 0.975),
    party = party_idx
  )
}

# Combine all parties
posterior_shares <- do.call(rbind, lapply(1:(K_fixed + K_trans), process_shares))

# Create observed proportions dataframe
observed_props <- data.frame(
  time = time_points[time_idx],
  party = rep(1:(K_fixed + K_trans), each = N),
  prop = as.vector(t(y_obs)) / n_resp,
  n_resp = rep(n_resp, K_fixed + K_trans)
)

# Filter out non-reported observations
observed_props <- observed_props[observed_props$prop > 0, ]

tmp <- f
true_shares <- exp(tmp - log(exp(tmp) %*% ones))
true_shares_df <- as.data.frame(true_shares) %>%
  mutate(time_point = row_number()) %>%
  pivot_longer(cols = -time_point,
               names_to = "party",
               values_to = "share") %>%
  mutate(party = as.numeric(gsub("V", "", party)))

# Plot for each party
ggplot() +
  # Posterior estimates
  geom_ribbon(data = posterior_shares,
              aes(x = time, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "blue") +
  geom_line(data = posterior_shares,
            aes(x = time, y = mean),
            color = "blue") +
  # True shares
  geom_line(data = true_shares_df,
             aes(x = time_point, y = share),
             color = "red", alpha = 0.5) +
  # Observed data points
  geom_point(data = observed_props,
             aes(x = time, y = prop, size = n_resp),
             alpha = 0.3) +
  facet_wrap(~party, scales = "free_y") +
  labs(title = "Vote Shares Over Time: True vs Estimated",
       subtitle = "Blue line/band = posterior estimates, Red points = true shares",
       x = "Time",
       y = "Vote Share",
       size = "Sample Size") +
  theme_minimal()



















# Check for parameter correlations
pairs_plot <- bayesplot::mcmc_pairs(fit$draws(c("alpha", "rho", "transition_rate")))
print(pairs_plot)

energy <- bayesplot::mcmc_nuts_energy(fit$draws())
print(energy)



# Basic diagnostics
fit$diagnostic_summary()
fit$summary()

# Extract and plot results
draws_df <- fit$draws(format = "draws_df")

# Plot estimated vs true transition weights for first transitioning party
ggplot() +
  geom_line(data = data.frame(
    time = time_points,
    weight = with(stan_data, 
                  party_weight(time_points, t_emerge[1], t_disappear[1], 1))),
    aes(x = time, y = weight), color = "blue") +
  geom_ribbon(data = data.frame(
    time = time_points,
    q05 = apply(draws_df[,paste0("transition_weights[1,", 1:T, "]")], 2, quantile, 0.05),
    q95 = apply(draws_df[,paste0("transition_weights[1,", 1:T, "]")], 2, quantile, 0.95)),
    aes(x = time, ymin = q05, ymax = q95), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Transition Weights - Party 1",
       x = "Time",
       y = "Weight")

# Helper function for weight calculation
party_weight <- function(t, t_emerge, t_disappear, rate) {
  emergence <- plogis(rate * (t - t_emerge))
  survival <- plogis(-rate * (t - t_disappear))
  emergence * survival
}




# Extract posterior draws for f and transition weights for first transitioning party (k=1)
draws_f <- fit$summary("f") %>%
  mutate(
    t = as.integer(str_match("\\[(\\d+),")[,2]),
    i = str_match(",(\\d+)")[,2]
  )
  
draws_w <- fit$draws(paste0("transition_weights"), format="draws_df")

# Create plot data
plot_data <- data.frame(
  time = time_points,
  f_mean = apply(draws_f, 2, mean),
  f_lower = apply(draws_f, 2, quantile, 0.025),
  f_upper = apply(draws_f, 2, quantile, 0.975),
  w_mean = apply(draws_w, 2, mean),
  w_lower = apply(draws_w, 2, quantile, 0.025),
  w_upper = apply(draws_w, 2, quantile, 0.975)
)

# Create scatter plot of f vs weight at each time point
ggplot(plot_data) +
  geom_point(aes(x = w_mean, y = f_mean, color = time)) +
  geom_errorbar(aes(x = w_mean, ymin = f_lower, ymax = f_upper), alpha = 0.2) +
  geom_errorbarh(aes(y = f_mean, xmin = w_lower, xmax = w_upper), alpha = 0.2) +
  scale_color_viridis_c() +
  labs(title = "Correlation between Transition Weight and Latent Vote Share",
       x = "Transition Weight",
       y = "Latent Vote Share (f)",
       color = "Time") +
  theme_minimal() +
  # Add correlation coefficient annotation
  annotate("text", x = min(plot_data$w_mean), y = max(plot_data$f_mean),
           label = paste("Correlation:", 
                         round(cor(plot_data$w_mean, plot_data$f_mean), 3)),
           hjust = 0, vjust = 1)

# Time series plot showing both f and w
ggplot(plot_data) +
  geom_ribbon(aes(x = time, ymin = f_lower, ymax = f_upper), alpha = 0.2, fill = "blue") +
  geom_line(aes(x = time, y = f_mean, color = "Latent Vote Share")) +
  geom_ribbon(aes(x = time, ymin = w_lower, ymax = w_upper), alpha = 0.2, fill = "red") +
  geom_line(aes(x = time, y = w_mean, color = "Transition Weight")) +
  scale_color_manual(values = c("blue", "red"), name = "Parameter") +
  labs(title = "Transition Weight and Latent Vote Share Over Time",
       x = "Time",
       y = "Value") +
  theme_minimal()




