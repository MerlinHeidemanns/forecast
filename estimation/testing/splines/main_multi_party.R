# Stan model for compositional splines with multinomial sampling
stan_code <- "
data {
  int<lower=1> N;          // number of observations
  int<lower=1> K;          // number of basis functions
  int<lower=2> P;          // number of parties
  vector[N] x;             // time
  array[N, P] int y;       // vote counts for P parties
  matrix[N, K] B;          // B-spline basis matrix
  real<lower=0> tau;       // smoothing parameter
}

transformed data {
  array[N] int totalVotes;
  for (n in 1:N) {
    totalVotes[n] = sum(y[n]);
  }
}

parameters {
  matrix[K, P-1] beta;    // spline coefficients for P-1 parties (relative to reference)
}

transformed parameters {
  matrix[N, P] mu;        // fitted values on simplex
  matrix[N, P] logits;    // log-odds relative to reference party
  
  // Set reference party (first party) to 0
  logits[,1] = rep_vector(0, N);
  
  // Compute logits for other parties
  logits[,2:P] = B * beta;
  
  // Transform to simplex using softmax
  for (n in 1:N) {
    mu[n] = softmax(logits[n]')';
  }
}

model {
  // Second differences smoothing prior for each party
  for(p in 1:(P-1)) {
    for (k in 3:K) {
      (beta[k, p] - 2 * beta[k-1, p] + beta[k-2, p]) ~ normal(0, tau);
    }
  }
  
  // Multinomial likelihood
  for (n in 1:N) {
    y[n] ~ multinomial(mu[n]');
  }
}

generated quantities {
  matrix[N, P] pred_probs;  // predicted probabilities
  array[N, P] int y_rep;         // posterior predictive samples
  
  // Generate posterior predictions
  for (n in 1:N) {
    pred_probs[n] = softmax(logits[n]')';
    y_rep[n] = multinomial_rng(pred_probs[n]', totalVotes[n]);
  }
}
"

writeLines(stan_code, "estimation/testing/splines/compositional_spline.stan")

# Generate synthetic data
set.seed(123)
N <- 20  # quarterly data over 50 years
x <- seq(1970, 2040, length.out = N)
P <- 3    # number of parties

# True trends on logit scale (relative to first party)
true_logits <- function(x) {
  # Party 2 vs Party 1 (declining)
  p2 <- -0.2 - 0.01 * (x - 1970) 
  
  # Party 3 vs Party 1 (rising)
  p3 <- -2 + 0.03 * (x - 1970)
  
  return(cbind(0, p2, p3))  # first party is reference
}

# Convert to probabilities using softmax
true_probs <- t(apply(true_logits(x), 1, function(l) {
  exp(l) / sum(exp(l))
}))

# Generate multinomial samples
n_votes <- rpois(N, lambda = 1000)  # varying sample sizes
y <- t(sapply(1:N, function(i) {
  rmultinom(1, n_votes[i], true_probs[i,])
}))

# Create B-spline basis
K <- 15
B <- bs(x, df = K, degree = 3, intercept = TRUE)

# Prepare data for Stan
stan_data <- list(
  N = N,
  K = K,
  P = P,
  x = x,
  y = y,
  B = B,
  tau = 0.1
)

# Compile and fit model
mod <- cmdstan_model("estimation/testing/splines/compositional_spline.stan")
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Extract posterior samples
draws <- fit$draws("pred_probs", format = "matrix")

# Calculate posterior means and intervals for each party
results_list <- list()
party_names <- c("Reference Party", "Party 2", "Party 3")

for(p in 1:P) {
  # Extract columns for this party
  prob_cols <- grep(paste0("pred_probs\\[.*,", p, "\\]"), colnames(draws))
  prob_mean <- apply(draws[, prob_cols], 2, mean)
  prob_lower <- apply(draws[, prob_cols], 2, quantile, 0.025)
  prob_upper <- apply(draws[, prob_cols], 2, quantile, 0.975)
  
  # Calculate observed proportions
  obs_props <- y[,p] / rowSums(y)
  
  results_list[[p]] <- data.frame(
    x = x,
    y = obs_props,
    fit = prob_mean,
    lower = prob_lower,
    upper = prob_upper,
    true = true_probs[,p],
    party = party_names[p]
  )
}

# Combine results
results_df <- do.call(rbind, results_list)

# Create plot
ggplot(results_df, aes(x = x, color = party)) +
  geom_point(aes(y = y), alpha = 0.3) +
  geom_line(aes(y = fit), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = party), alpha = 0.2, color = NA) +
  geom_line(aes(y = true), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Party Vote Shares",
       subtitle = "Points: observed proportions, Lines: fitted trends, Dashed: true probabilities",
       x = "Year",
       y = "Vote Share") +
  scale_y_continuous(labels = scales::percent)

