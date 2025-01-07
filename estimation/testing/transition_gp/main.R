library(cmdstanr)
library(tidyverse)
library(mgcv)
library(posterior)
library(bayesplot)
library(patchwork)

# Set random seed for reproducibility
set.seed(12345)

#######################
# Simulation Functions
#######################

simulate_gp_trends <- function(n_timepoints, n_parties, rho=5, sigma=0.5) {
  time_scaled <- seq(0, 1, length.out=n_timepoints)
  trends <- matrix(0, nrow=n_timepoints, ncol=n_parties)
  
  # Simulate correlated GPs
  Sigma <- matrix(0.5, n_parties, n_parties)
  diag(Sigma) <- 1
  eps <- MASS::mvrnorm(n_timepoints, mu=rep(0, n_parties), Sigma=Sigma)
  
  for(i in 1:n_parties) {
    gp <- gam(eps[,i] ~ s(time_scaled, k=20))
    trends[,i] <- predict(gp)
  }
  
  # Center and scale
  trends <- scale(trends)
  return(trends)
}

create_vote_share_plots <- function(plot_data, survey_data, time_points, t_emerge, t_disappear) {
  # Main trajectory plot
  p1 <- ggplot(plot_data, aes(x=time, color=party)) +
    # Add shaded regions for pre-emergence and post-disappearance
    geom_rect(aes(xmin=-Inf, xmax=t_emerge, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    geom_rect(aes(xmin=t_disappear, xmax=Inf, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    # Add vertical lines for transitions
    geom_vline(xintercept=c(t_emerge, t_disappear), linetype='dashed', alpha=0.5) +
    geom_line(aes(y=true_prob), linetype="dashed", alpha=0.7) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=party), alpha=0.2) +
    geom_line(aes(y=pred_prob), size=1) +
    geom_point(data=survey_data, aes(y=vote_share), alpha=0.3, size=1) +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1") +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) +
    labs(title="Vote Share Trajectories",
         subtitle=paste0("Party transitions at t = ", t_emerge, " and t = ", t_disappear),
         x="Time", y="Vote Share") +
    theme_minimal() +
    theme(legend.position="bottom",
          panel.grid.minor=element_blank())
  
  # Weight function visualization
  weight_df <- data.frame(
    time = time_points,
    emergence = 1 / (1 + exp(-(time_points - t_emerge) * 0.5)),
    survival = 1 / (1 + exp((time_points - t_disappear) * 0.5)),
    weight = 1 / (1 + exp(-(time_points - t_emerge) * 0.5)) * 
      1 / (1 + exp((time_points - t_disappear) * 0.5))
  ) %>%
    pivot_longer(cols=c(emergence, survival, weight),
                 names_to="component", values_to="value")
  
  p2 <- ggplot(weight_df, aes(x=time, y=value, color=component)) +
    geom_rect(aes(xmin=-Inf, xmax=t_emerge, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    geom_rect(aes(xmin=t_disappear, xmax=Inf, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    geom_vline(xintercept=c(t_emerge, t_disappear), linetype='dashed', alpha=0.5) +
    geom_line(size=1) +
    scale_y_continuous(limits=c(0,1)) +
    scale_color_manual(values=c("darkred", "purple", "darkblue")) +
    labs(title="Transition Weight Functions",
         x="Time", y="Weight") +
    theme_minimal() +
    theme(panel.grid.minor=element_blank())
  
  # Residual plot
  p3 <- ggplot(plot_data, aes(x=time, y=residual, color=party)) +
    geom_rect(aes(xmin=-Inf, xmax=t_emerge, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3) +
    geom_rect(aes(xmin=t_disappear, xmax=Inf, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3) +
    geom_vline(xintercept=c(t_emerge, t_disappear), linetype='dashed', alpha=0.5) +
    geom_hline(yintercept=0, linetype='dashed') +
    geom_point(alpha=0.5) +
    scale_color_brewer(palette="Set1") +
    labs(title="Standardized Residuals",
         x="Time", y="(O-E)/sqrt(E)") +
    theme_minimal() +
    theme(legend.position="none",
          panel.grid.minor=element_blank())
  
  # True vote shares plot
  p4 <- ggplot(plot_data, aes(x=time, color=party)) +
    geom_rect(aes(xmin=-Inf, xmax=t_emerge, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    geom_rect(aes(xmin=t_disappear, xmax=Inf, ymin=-Inf, ymax=Inf), 
              fill='grey90', alpha=0.3, inherit.aes=FALSE) +
    geom_vline(xintercept=c(t_emerge, t_disappear), linetype='dashed', alpha=0.5) +
    geom_line(aes(y=true_prob), size=1) +
    scale_color_brewer(palette="Set1") +
    scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) +
    labs(title="True Vote Shares",
         x="Time", y="Vote Share") +
    theme_minimal() +
    theme(legend.position="bottom",
          panel.grid.minor=element_blank())
  
  combined_plot <- (p1 + p2 + p3 + p4) +
    plot_layout(heights=c(2,1,1,1)) +
    plot_annotation(
      title="Vote Share Model Analysis",
      subtitle="Showing trajectories, transition functions, and model diagnostics",
      theme=theme_minimal()
    )
  
  return(combined_plot)
}
########################
# Simulation Parameters
########################

n_parties <- 4
n_timepoints <- 50
t_emerge <- 15
t_disappear <- 35
trans_party_idx <- 2
time_points <- seq(1, n_timepoints)

########################
# Generate Data
########################

# Simulate true trends
true_trends <- simulate_gp_trends(n_timepoints, n_parties-1)
true_trends <- cbind(true_trends, 0)  # add reference party

large_neg <- -100  # Same as in Stan model
w <- sapply(time_points, function(t) {
  emergence = 1 / (1 + exp(-(t - t_emerge) * 0.5))
  survival = 1 / (1 + exp((t - t_disappear) * 0.5))
  weight = emergence * survival
  # Interpolate between actual logit and large negative value
  weight * true_trends[t,trans_party_idx] + (1 - weight) * large_neg
})
true_trends[,trans_party_idx] <- unlist(w)

# Convert to probabilities
softmax <- function(x) exp(x) / sum(exp(x))
true_probs <- t(apply(true_trends, 1, softmax))

# Generate survey data
n_surveys <- 100
survey_times <- sample(1:n_timepoints, n_surveys, replace=TRUE)
n_respondents <- rpois(n_surveys, lambda=500)

# Generate counts
survey_data <- matrix(0, nrow=n_surveys, ncol=n_parties)
for(i in 1:n_surveys) {
  survey_data[i,] <- rmultinom(1, n_respondents[i], true_probs[survey_times[i],])
}

########################
# Fit Model
########################

stan_data <- list(
  N = n_surveys,
  K = n_parties,
  T = n_timepoints,
  time_points = time_points,
  time_idx = survey_times,
  n_resp = n_respondents,
  y = survey_data,
  t_emerge = t_emerge,
  t_disappear = t_disappear,
  trans_party_idx = trans_party_idx
)

# Compile and fit model
mod <- cmdstan_model("estimation/testing/transition_gp/gp_model_entering.stan",
                     stanc_options = list("O1"), compile = TRUE)
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  adapt_delta = 0.9,
  max_treedepth = 10
)

# Check diagnostics
fit$diagnostic_summary()
fit$summary(c("alpha", "rho", "transition_rate"))

########################
# Analysis & Visualization
########################

# Extract posterior
draws_df <- fit$draws(format = "df")
vote_shares <- as_draws_matrix(draws_df) %>%
  subset_draws(variable = "vote_shares")

# Calculate summaries
vote_shares_mean <- matrix(
  apply(vote_shares, 2, mean),
  nrow = n_timepoints,
  ncol = n_parties
)
vote_shares_lower <- matrix(
  apply(vote_shares, 2, quantile, 0.025),
  nrow = n_timepoints,
  ncol = n_parties
)
vote_shares_upper <- matrix(
  apply(vote_shares, 2, quantile, 0.975),
  nrow = n_timepoints,
  ncol = n_parties
)

# Prepare plot data
plot_data <- data.frame(
  time = rep(time_points, n_parties),
  party = factor(rep(1:n_parties, each=n_timepoints)),
  true_prob = as.vector(true_probs),
  pred_prob = as.vector(vote_shares_mean),
  lower = as.vector(vote_shares_lower),
  upper = as.vector(vote_shares_upper)
) %>%
  group_by(party, time) %>%
  mutate(
    residual = (true_prob - pred_prob) / sqrt(pred_prob * (1-pred_prob))
  )

# Prepare survey data for plotting
survey_data <- data.frame(
  time = rep(time_points[survey_times], n_parties),
  party = factor(rep(1:n_parties, each=length(survey_times))),
  vote_share = as.vector(t(sweep(survey_data, 1, n_respondents, "/")))
)

# Create and save visualization
p <- create_vote_share_plots(plot_data, survey_data, time_points, t_emerge, t_disappear)
ggsave("vote_share_analysis.pdf", p, width=12, height=15, units="in")

# MCMC diagnostics
mcmc_trace(fit$draws(c("alpha", "rho", "transition_rate")))

# Calculate and print R-hat values
print("R-hat values:")
rhat <- fit$summary(c("alpha", "rho", "transition_rate"))$rhat
print(rhat)