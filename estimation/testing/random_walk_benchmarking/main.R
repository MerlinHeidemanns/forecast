# Load required packages
library(cmdstanr)
library(posterior)
library(ggplot2)
library(dplyr)
library(bayesplot)
library(Matrix)

# Generate synthetic data
set.seed(123)
N <- 100      # time points
D <- 3        # dimensions
rho <- 0.5    # correlation between dimensions

# Create innovation covariance matrix
Sigma_latent <- matrix(rho, D, D)
diag(Sigma_latent) <- 1
Sigma_latent <- Sigma_latent * 0.1  # scale innovations

# Generate true latent states
x_true <- matrix(0, N, D)
innovations <- MASS::mvrnorm(N-1, rep(0, D), Sigma_latent)
x_true[1,] <- rnorm(D, 0, 0.1)
for(t in 2:N) {
  x_true[t,] <- x_true[t-1,] + innovations[t-1,]
}

# Generate heterogeneous measurement errors and observations
se <- matrix(runif(N*D, 0.1, 0.3), N, D)
y <- matrix(NA, N, D)
for(t in 1:N) {
  y[t,] <- rnorm(D, x_true[t,], se[t,])
}

# Prepare data for Stan
stan_data <- list(
  N = N,
  D = D,
  y = y,
  se = se
)

# Basic centered parameterization
basic_model <- write_stan_file("
data {
  int<lower=0> N;         // number of time points
  int<lower=0> D;         // dimension of state
  matrix[N, D] y;         // observations
  matrix[N, D] se;        // known measurement standard errors
}
parameters {
  cholesky_factor_corr[D] L_Omega;  // Cholesky of correlation matrix
  vector<lower=0>[D] sigma_latent;   // innovation standard deviations
  matrix[N, D] x;                    // latent states
}
transformed parameters {
  matrix[D, D] Sigma_latent;
  {
    matrix[D, D] L = diag_pre_multiply(sigma_latent, L_Omega);
    Sigma_latent = L * L';
  }
}
model {
  // Priors
  L_Omega ~ lkj_corr_cholesky(2);
  sigma_latent ~ cauchy(0, 2.5);
  
  // Prior for initial state
  x[1] ~ multi_normal(rep_vector(0, D), diag_matrix(rep_vector(10.0, D)));
  
  // Latent state dynamics
  for (t in 2:N) {
    x[t] ~ multi_normal(x[t-1], Sigma_latent);
  }
  
  // Measurement model with known SEs
  for (t in 1:N) {
    for (d in 1:D) {
      y[t,d] ~ normal(x[t,d], se[t,d]);
    }
  }
}
")

# Non-centered parameterization
noncentered_model <- write_stan_file("
data {
  int<lower=0> N;
  int<lower=0> D;
  matrix[N, D] y;
  matrix[N, D] se;
}
parameters {
  cholesky_factor_corr[D] L_Omega;
  vector<lower=0>[D] sigma_latent;
  matrix[N-1, D] eps_latent;  // standardized innovations
  row_vector[D] x_init;           // initial state
}
transformed parameters {
  matrix[D, D] L;             // Cholesky of innovation covariance
  matrix[N, D] x;             // latent states
  
  // Construct Cholesky factor of covariance matrix
  L = diag_pre_multiply(sigma_latent, L_Omega);
  
  // Reconstruct states
  x[1] = x_init;
  for (t in 2:N) {
    x[t] = x[t-1] + (L * eps_latent[t-1]')';
  }
}
model {
  // Priors
  L_Omega ~ lkj_corr_cholesky(2);
  sigma_latent ~ cauchy(0, 2.5);
  x_init ~ multi_normal(rep_vector(0, D), diag_matrix(rep_vector(10.0, D)));
  
  // Standard normal prior for innovations
  for (t in 1:(N-1)) {
    eps_latent[t] ~ std_normal();
  }
  
  // Measurement model
  for (t in 1:N) {
    for (d in 1:D) {
      y[t,d] ~ normal(x[t,d], se[t,d]);
    }
  }
}
generated quantities {
  matrix[D, D] Sigma_latent = L * L';
  matrix[D, D] Omega = L_Omega * L_Omega';  // correlation matrix
}
")

# Cholesky parameterization for full trajectory
cholesky_model <- write_stan_file("
data {
  int<lower=0> N;
  int<lower=0> D;
  matrix[N, D] y;
  matrix[N, D] se;
}
transformed data {
  matrix[N,N] K;
  matrix[N,N] L_K;
  // Construct random walk covariance structure
  for (i in 1:N) {
    for (j in 1:N) {
      K[i,j] = min(i,j);
    }
  }
  L_K = cholesky_decompose(K);
}
parameters {
  cholesky_factor_corr[D] L_Omega;
  vector<lower=0>[D] sigma_latent;
  matrix[N, D] eta;          // std normal variables
  vector[D] x_init;          // initial state
}
transformed parameters {
  matrix[N, D] x;
  matrix[D, D] L = diag_pre_multiply(sigma_latent, L_Omega);
  
  // Construct full trajectory
  for (d in 1:D) {
    x[,d] = x_init[d] + (L[d,d] * L_K * eta[,d]);
  }
}
model {
  // Priors
  L_Omega ~ lkj_corr_cholesky(2);
  sigma_latent ~ cauchy(0, 2.5);
  x_init ~ multi_normal(rep_vector(0, D), diag_matrix(rep_vector(10.0, D)));
  
  // Standard normal prior for innovations
  for (t in 1:N) {
    eta[t] ~ std_normal();
  }
  
  // Measurement model
  for (t in 1:N) {
    for (d in 1:D) {
      y[t,d] ~ normal(x[t,d], se[t,d]);
    }
  }
}
generated quantities {
  matrix[D, D] Sigma_latent = L * L';
  matrix[D, D] Omega = L_Omega * L_Omega';
}
")

# Function to compile model
compile_model <- function(file, name) {
  cat(sprintf("Compiling %s model...\n", name))
  mod <- cmdstan_model(file, 
                      cpp_options = list(stan_threads = TRUE),
                      stanc_options = list("O1"))
  return(mod)
}

# Compile all models
models <- list(
  basic = compile_model(basic_model, "Basic"),
  noncentered = compile_model(noncentered_model, "Non-centered"),
  cholesky = compile_model(cholesky_model, "Cholesky")
)

# Function to run sampling with timing and diagnostics
run_sampling <- function(model, data, name) {
  cat(sprintf("Running %s model...\n", name))
  
  fit <- model$sample(
    data = data,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    threads_per_chain = 2,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 500,
    adapt_delta = 0.9,
    max_treedepth = 12,
    init = function() list(
      sigma_latent = rep(0.1, data$D),
      L_Omega = diag(data$D)
    )
  )
  
  # Extract diagnostics
  diagnostics <- list(
    time = fit$time()$total,
    divergent = sum(fit$diagnostic_summary()$num_divergent),
    treedepth = sum(fit$diagnostic_summary()$num_max_treedepth),
    n_eff = summarise_draws(fit$draws(), "ess_bulk")$ess_bulk,
    rhat = summarise_draws(fit$draws(), "rhat")$rhat
  )
  
  return(list(fit = fit, diagnostics = diagnostics))
}

# Run benchmarks
results <- list()
for (name in names(models)) {
  results[[name]] <- run_sampling(models[[name]], stan_data, name)
}

# Create summary dataframe
summary_df <- data.frame(
  Model = names(results),
  Time = sapply(results, function(x) x$diagnostics$time),
  Divergences = sapply(results, function(x) x$diagnostics$divergent),
  Max_Treedepth = sapply(results, function(x) x$diagnostics$treedepth),
  Min_n_eff = sapply(results, function(x) min(x$diagnostics$n_eff)),
  Max_Rhat = sapply(results, function(x) max(x$diagnostics$rhat))
)

# Print results
cat("\nBenchmark Results:\n")
print(summary_df)

# Plot correlation matrices for non-centered model
fit_nc <- results$noncentered$fit
Omega_draws <- fit_nc$draws("Omega")
Omega_mean <- matrix(colMeans(as_draws_matrix(Omega_draws)), D, D)

# Plot estimated correlations vs true
correlation_plot <- ggplot(data.frame(
  True = Sigma_latent[upper.tri(Sigma_latent)],
  Estimated = Omega_mean[upper.tri(Omega_mean)]
)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "True vs Estimated Correlations",
       x = "True Correlation",
       y = "Estimated Correlation")

# Save results
write.csv(summary_df, "cmdstan_mv_rw_results.csv", row.names = FALSE)
ggsave("correlation_comparison.png", correlation_plot, width = 8, height = 6)