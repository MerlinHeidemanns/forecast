library(cmdstanr)
library(MASS)
library(tidyverse)
library(posterior)
library(bayesplot)

# Simulation parameters
set.seed(123)
K <- 4          # number of original parties
T <- 50         # time periods
t_transition <- 25  # when new party emerges
N_pre <- 10000   # respondents per poll pre-transition
N_post <- 10000  # respondents per poll post-transition

# Generate true correlation structure
true_L_base <- t(chol(rWishart(1, K, diag(K))[,,1]))  # K dimensions for K parties plus residual
true_gamma <- rnorm(K, 0, 0.3)    # correlations of new party with K existing parties plus residual
true_gamma <- true_gamma * sqrt(1/sum(true_gamma^2))  # scale to ensure validity
true_lambda <- 0.7

# Generate random walk parameters
sigma_pre <- rep(0.1, K)          # K parties plus residual
sigma_post <- c(sigma_pre, 0.15)  # add volatility for new party

# Function to construct post-transition Cholesky
construct_L_post <- function(L_base, gamma, sigma_post) {
  K <- nrow(L_base) + 1
  L_post <- matrix(0, K, K)
  
  # Copy base structure
  for(i in 1:(K-1)) {
    for(j in 1:(K-1)) {
      L_post[i,j] <- sigma_post[i] * L_base[i,j]
    }
  }
  
  # Add new party row
  for(i in 1:(K-1)) {
    L_post[K,i] <- sigma_post[K] * gamma[i]
  }
  L_post[K,K] <- sigma_post[K] * sqrt(1 - sum(gamma^2))
  
  return(L_post)
}

# Simulate latent states
simulate_states <- function(T, K, t_transition, L_pre, L_post) {
  # Pre-transition states
  states_pre <- matrix(0, T, K)
  for(t in 2:t_transition) {
    states_pre[t,] <- states_pre[t-1,] + L_pre[1:K, 1:K] %*% c(rnorm(K, 0.1))
  }
  
  # Post-transition states
  states_post <- matrix(0, T-t_transition, K + 1)
  states_post[1,1:(K)] <- states_pre[t_transition,]
  states_post[1,K + 1] <- states_pre[t_transition,K] * true_lambda
  
  for(t in 2:(T-t_transition)) {
    states_post[t,] <- states_post[t-1,] + L_post %*% c(rnorm(K + 1))
  }
  
  return(list(pre=states_pre, post=states_post))
}

# Transform to probabilities and generate multinomial observations
softmax <- function(x) {
  exp(x) / sum(exp(x))
}

# Simulate data
L_pre <- construct_L_post(true_L_base, true_gamma, sigma_pre)
L_post <- construct_L_post(true_L_base, true_gamma, sigma_post)

states <- simulate_states(T, K, t_transition, L_pre, L_post)

# Generate observations
Y_pre <- matrix(0, t_transition, K+1)   # K parties plus residual (K+1 total)
Y_post <- matrix(0, T-t_transition, K+2) # K parties plus new party plus residual (K+2 total)

# Pre-transition: K parties plus residual
for(t in 1:t_transition) {
  logits_pre <- c(0, states$pre[t,])  # First element is baseline (0), followed by K logits (including residual)
  probs <- softmax(logits_pre)        # This gives K+1 probabilities
  Y_pre[t,] <- rmultinom(1, N_pre, probs)
}

# Post-transition: K parties plus new party plus residual
for(t in 1:(T-t_transition)) {
  logits_post <- c(0, states$post[t,])  # First element is baseline (0), followed by K+1 logits
  probs <- softmax(logits_post)         # This gives K+2 probabilities
  Y_post[t,] <- rmultinom(1, N_post, probs)
}

# Prepare Stan data
stan_data <- list(
  K = K,
  T = T,
  t_transition = t_transition,
  N_pre = N_pre,
  N_post = N_post,
  Y_pre = Y_pre,
  Y_post = Y_post,
  scale_prior = 0.4
)

# Compile and fit model
mod <- cmdstan_model("estimation/testing/transition/model.stan",
                     stanc_options = list("O1"))

fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500
)

# Basic diagnostics
fit$diagnostic_summary()
fit$summary(c("lambda", paste0("gamma[", 1:3, "]")))

# Extract draws in a tidy format
draws_df <- fit$draws("lambda") %>%
  posterior::as_draws_df()

# Plot diagnostics
# 1. Trace plots for key parameters
mcmc_trace(draws_df, 
           regex_pars = "lambda")

# 2. Compare true vs estimated lambda
p1 <- ggplot(draws_df, aes(x = lambda)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = true_lambda, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Posterior of lambda",
       x = "lambda",
       y = "Count")

# 3. Compare true vs estimated gamma
gamma_draws <- draws_df %>%
  select(starts_with("gamma")) %>%
  pivot_longer(everything(), 
               names_to = "parameter",
               values_to = "value")

p2 <- ggplot(gamma_draws, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(data = tibble(parameter = paste0("gamma[", 1:3, "]"),
                           true_value = true_gamma),
             aes(xintercept = true_value),
             color = "red", size = 1) +
  facet_wrap(~parameter, ncol = 1) +
  theme_minimal() +
  labs(title = "Posterior of gamma parameters",
       x = "value",
       y = "Count")

# 4. Plot latent share of new party
N_latent_summary <- fit$summary(paste0("N_latent[", 1:t_transition, "]"))

p3 <- ggplot(N_latent_summary, aes(x = 1:t_transition, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Estimated latent share of new party",
       x = "Time",
       y = "Share")

# Convergence diagnostics
# Rhat plot for all parameters
mcmc_rhat(fit$summary()$rhat) +
  theme_minimal() +
  labs(title = "Rhat values for all parameters")

# Save plots if needed
ggsave("lambda_posterior.png", p1, width = 8, height = 6)
ggsave("gamma_posterior.png", p2, width = 8, height = 10)
ggsave("latent_share.png", p3, width = 8, height = 6)

# Extract posterior predictive checks if needed
# (Add specific posterior predictive checks based on your needs)

# Print summary of key parameters
cat("\nTrue lambda:", true_lambda, "\n")
cat("Estimated lambda (mean):", 
    mean(draws_df$lambda), 
    "95% CI:", quantile(draws_df$lambda, c(0.025, 0.975)), "\n")

for(i in 1:3) {
  cat("\nTrue gamma[", i, "]:", true_gamma[i], "\n")
  cat("Estimated gamma[", i, "] (mean):", 
      mean(draws_df[[paste0("gamma[", i, "]")]]),
      "95% CI:", quantile(draws_df[[paste0("gamma[", i, "]")]], c(0.025, 0.975)), "\n")
}




analyze_divergences <- function(fit) {
  # Extract draws and divergence info
  draws_df <- fit$draws(format = "draws_df")
  
  # 1. Pairs plots for key parameters
  # Focus on transition parameters and first few states
  key_params <- c("lambda", paste0("gamma[", 1:K, "]"),
                  "state_post[1,2]", "state_post[1,3]", "state_post[1,4]")
  
  p1 <- mcmc_pairs(draws_df,
                   pars = key_params,
                   diag_fun = "hist",
                   off_diag_fun = c("hex", "size"),
                   condition = draws_df$.divergent)
  
  # 2. Parallel coordinates plot for divergent transitions
  # This helps identify problematic parameter subspaces
  p2 <- mcmc_parcoord(draws_df,
                      pars = key_params,
                      condition = draws_df$.divergent)
  
  # 3. State magnitude analysis
  # Create time series of state magnitudes
  state_vars <- grep("state_post", names(draws_df), value = TRUE)
  state_magnitudes <- draws_df %>%
    select(all_of(state_vars), .chain, .iteration, .divergent) %>%
    pivot_longer(all_of(state_vars),
                 names_to = "state",
                 values_to = "value") %>%
    group_by(.chain, .iteration) %>%
    summarize(max_magnitude = max(abs(value)),
              avg_magnitude = mean(abs(value)),
              .divergent = first(.divergent))
  
  p3 <- ggplot(state_magnitudes, 
               aes(x = max_magnitude, fill = factor(.divergent))) +
    geom_histogram(position = "dodge", bins = 50) +
    scale_fill_manual(values = c("grey70", "red")) +
    labs(title = "Distribution of Maximum State Magnitudes",
         x = "Maximum Absolute State Value",
         fill = "Divergent")
  
  # 4. Transition probabilities analysis
  pi_vars <- grep("pi_post", names(draws_df), value = TRUE)
  prob_extremes <- draws_df %>%
    select(all_of(pi_vars), .chain, .iteration, .divergent) %>%
    pivot_longer(all_of(pi_vars),
                 names_to = "probability",
                 values_to = "value") %>%
    group_by(.chain, .iteration) %>%
    summarize(min_prob = min(value),
              max_prob = max(value),
              .divergent = first(.divergent))
  
  p4 <- ggplot(prob_extremes, 
               aes(x = log10(min_prob), y = log10(1-max_prob), 
                   color = factor(.divergent))) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("grey50", "red")) +
    labs(title = "Probability Extremes",
         x = "log10(Minimum Probability)",
         y = "log10(1 - Maximum Probability)",
         color = "Divergent")
  
  # 5. Scale parameter relationships
  scale_vars <- c(paste0("sigma_pre[", 1:K, "]"), "sigma_post")
  scale_divergence <- draws_df %>%
    select(all_of(scale_vars), .chain, .iteration, .divergent) %>%
    pivot_longer(all_of(scale_vars),
                 names_to = "scale",
                 values_to = "value") %>%
    group_by(.chain, .iteration) %>%
    summarize(max_scale = max(value),
              min_scale = min(value),
              scale_ratio = max_scale / min_scale,
              .divergent = first(.divergent))
  
  p5 <- ggplot(scale_divergence,
               aes(x = scale_ratio, fill = factor(.divergent))) +
    geom_histogram(position = "dodge", bins = 50) +
    scale_fill_manual(values = c("grey70", "red")) +
    labs(title = "Distribution of Scale Parameter Ratios",
         x = "Max/Min Scale Ratio",
         fill = "Divergent")
  
  # Return list of plots
  list(
    pairs = p1,
    parallel = p2,
    state_magnitudes = p3,
    probability_extremes = p4,
    scale_ratios = p5
  )
}
p = analyze_divergences(fit)
