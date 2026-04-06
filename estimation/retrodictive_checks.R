################################################################################
# Retrodictive Checks
#
# Purpose: Assess model fit against training data and save benchmark JSON.
#   1. Election result recovery — posterior vs actual Zweitstimme
#   2. Poll residuals over time — systematic misfit detection
#   3. Posterior predictive checks — simulated vs observed poll shares
#
# Requires: data/spline_fit.rds (produced by main.R)
# Outputs:  estimation/plt/spline/retro_*.png
#           data/benchmarks/<model>_<timestamp>.json
#
# Usage:   source("estimation/retrodictive_checks.R")
################################################################################

library(tidyverse)
library(cmdstanr)
library(jsonlite)

source("estimation/theme.R")

PLT_DIR <- "estimation/plt/spline"
dir.create(PLT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create("data/benchmarks", recursive = TRUE, showWarnings = FALSE)


################################################################################
# Load Fitted Model
################################################################################

res <- readRDS("data/spline_fit.rds")

fit           <- res$fit
data_list     <- res$data_list
PARTIES       <- res$parties
period_breaks <- res$period_breaks
elections     <- res$elections
e_vote        <- res$e_vote
e_idx         <- res$e_idx
t_agg         <- res$t_agg
y_agg         <- res$y_agg
N_agg         <- res$N_agg

T_max <- data_list$T
P     <- data_list$P
K     <- data_list$K

y_obs_prop <- y_agg / rowSums(y_agg)
colnames(y_obs_prop) <- PARTIES


################################################################################
# 1. Election Result Recovery
################################################################################

message("=== Check 1: Election result recovery ===")

E <- nrow(e_vote)
election_checks <- list()
for (j in 1:E) {
  t_j <- e_idx[j]
  for (p in 1:P) {
    draws <- fit$draws(sprintf("pi[%d,%d]", t_j, p), format = "matrix")
    election_checks[[length(election_checks) + 1]] <- tibble(
      election_date = elections$election_date[j],
      party         = PARTIES[p],
      actual        = e_vote[j, p],
      median        = median(draws),
      q05 = quantile(draws, 0.05), q25 = quantile(draws, 0.25),
      q75 = quantile(draws, 0.75), q95 = quantile(draws, 0.95)
    )
  }
}

election_df <- bind_rows(election_checks) %>%
  mutate(
    party = factor(party, levels = names(PARTY_COLORS)),
    election_label = format(election_date, "%Y")
  )

p1 <- ggplot(election_df, aes(x = election_label, color = party)) +
  geom_linerange(aes(ymin = q05, ymax = q95), linewidth = 0.6, alpha = 0.4) +
  geom_linerange(aes(ymin = q25, ymax = q75), linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(y = median), size = 2) +
  geom_point(aes(y = actual), shape = 4, size = 3, stroke = 1.2) +
  facet_wrap(~party, scales = "free_y", ncol = 4) +
  scale_color_parties() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Election result recovery",
    subtitle = "Circles = posterior median, crosses = actual. Bars = 50%/90% CI.",
    y = "Vote share", x = "Election year"
  ) +
  theme_forecast() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.7)))

ggsave(file.path(PLT_DIR, "retro_elections.png"), p1, width = 14, height = 8, dpi = 200)
message("Saved: retro_elections.png")

## Compute election metrics
election_df <- election_df %>%
  mutate(covered_90 = actual >= q05 & actual <= q95,
         covered_50 = actual >= q25 & actual <= q75,
         abs_error  = abs(actual - median))

election_coverage <- election_df %>%
  summarize(
    coverage_90 = mean(covered_90),
    coverage_50 = mean(covered_50),
    mae         = mean(abs_error)
  )

election_by_party <- election_df %>%
  group_by(party) %>%
  summarize(
    coverage_90 = mean(covered_90),
    coverage_50 = mean(covered_50),
    mae         = mean(abs_error),
    .groups     = "drop"
  )

message(sprintf("  90%% coverage: %.0f%% | 50%% coverage: %.0f%% | MAE: %.3f",
                election_coverage$coverage_90 * 100,
                election_coverage$coverage_50 * 100,
                election_coverage$mae))


################################################################################
# 2. Poll Residuals Over Time
################################################################################

message("\n=== Check 2: Poll residuals over time ===")

residual_list <- list()
for (i in 1:N_agg) {
  t_i <- t_agg[i]
  for (p in 1:P) {
    draws <- fit$draws(sprintf("pi[%d,%d]", t_i, p), format = "matrix")
    residual_list[[length(residual_list) + 1]] <- tibble(
      date     = period_breaks[t_i],
      party    = PARTIES[p],
      observed = y_obs_prop[i, p],
      fitted   = median(draws),
      residual = y_obs_prop[i, p] - median(draws)
    )
  }
}

residual_df <- bind_rows(residual_list) %>%
  mutate(party = factor(party, levels = names(PARTY_COLORS)))

p2 <- ggplot(residual_df, aes(x = date, y = residual, color = party)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_point(size = 0.4, alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 0.7) +
  facet_wrap(~party, ncol = 4, scales = "free_y") +
  scale_color_parties() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Poll residuals over time",
    subtitle = "Observed poll share minus posterior median. LOESS smoother shows systematic misfit.",
    y = "Residual (obs - fitted)"
  ) +
  theme_forecast() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.7)))

ggsave(file.path(PLT_DIR, "retro_residuals.png"), p2, width = 14, height = 8, dpi = 200)
message("Saved: retro_residuals.png")

residual_by_party <- residual_df %>%
  group_by(party) %>%
  summarize(
    mean_residual = mean(residual),
    sd_residual   = sd(residual),
    rmse          = sqrt(mean(residual^2)),
    .groups       = "drop"
  )

print(residual_by_party)


################################################################################
# 3. Posterior Predictive Check
################################################################################

message("\n=== Check 3: Posterior predictive check ===")

phi_draws <- as.vector(fit$draws("phi", format = "matrix"))
n_draws   <- length(phi_draws)
n_ppc     <- min(N_agg, 200)
n_rep     <- 100

set.seed(42)
ppc_idx <- sort(sample(1:N_agg, n_ppc))
ppc_list <- list()

for (ii in seq_along(ppc_idx)) {
  i   <- ppc_idx[ii]
  t_i <- t_agg[i]
  n_i <- sum(y_agg[i, ])

  pi_mat <- matrix(NA, n_draws, P)
  for (p in 1:P)
    pi_mat[, p] <- fit$draws(sprintf("pi[%d,%d]", t_i, p), format = "matrix")[, 1]

  draw_ids <- sample(1:n_draws, n_rep, replace = TRUE)
  for (r in 1:n_rep) {
    d <- draw_ids[r]
    alpha_conc <- pi_mat[d, ] * phi_draws[d]
    theta <- rgamma(P, shape = alpha_conc, rate = 1)
    theta <- theta / sum(theta)
    y_rep <- rmultinom(1, size = n_i, prob = theta)

    for (p in 1:P) {
      ppc_list[[length(ppc_list) + 1]] <- tibble(
        obs_idx = i, date = period_breaks[t_i], party = PARTIES[p],
        observed = y_obs_prop[i, p], simulated = y_rep[p, 1] / n_i
      )
    }
  }
  if (ii %% 50 == 0) message(sprintf("  PPC: %d/%d done", ii, n_ppc))
}

ppc_df <- bind_rows(ppc_list) %>%
  mutate(party = factor(party, levels = names(PARTY_COLORS)))

p3 <- ggplot() +
  geom_density(data = ppc_df, aes(x = simulated, group = obs_idx),
               color = "grey70", linewidth = 0.15, alpha = 0.1) +
  geom_density(data = ppc_df %>% distinct(obs_idx, party, observed),
               aes(x = observed), color = "black", linewidth = 0.7) +
  facet_wrap(~party, scales = "free", ncol = 4) +
  labs(
    title    = "Posterior predictive check",
    subtitle = "Grey = simulated poll share distributions, black = observed.",
    x = "Vote share", y = "Density"
  ) +
  theme_forecast()

ggsave(file.path(PLT_DIR, "retro_ppc.png"), p3, width = 14, height = 8, dpi = 200)
message("Saved: retro_ppc.png")

ppc_calibration <- ppc_df %>%
  group_by(obs_idx, party) %>%
  summarize(q05_sim = quantile(simulated, 0.05), q95_sim = quantile(simulated, 0.95),
            observed = first(observed), .groups = "drop") %>%
  mutate(covered = observed >= q05_sim & observed <= q95_sim) %>%
  group_by(party) %>%
  summarize(coverage_90 = mean(covered), .groups = "drop")

message("\nPPC 90% interval coverage by party:")
print(ppc_calibration)


################################################################################
# Save Benchmark JSON
################################################################################

message("\n=== Saving benchmark ===")

## Read Stan model source
stan_code <- paste(readLines("estimation/stan/spline.stan"), collapse = "\n")

## Sampler diagnostics
diag_summary <- fit$diagnostic_summary()

## Parameter summaries
tau_summary <- fit$summary("tau")
phi_summary <- fit$summary("phi")

benchmark <- list(
  metadata = list(
    model_name   = "spline",
    stan_file    = "estimation/stan/spline.stan",
    timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    r_version    = paste0(R.version$major, ".", R.version$minor),
    cmdstan_version = cmdstanr::cmdstan_version()
  ),
  model = list(
    stan_code = stan_code,
    data_config = list(
      layer_period   = "14 days",
      start_date     = "1998-01-01",
      parties        = PARTIES,
      n_parties      = P,
      n_basis        = K,
      n_time_periods = T_max,
      n_observations = N_agg,
      n_elections    = E,
      knot_placement = "election_dates",
      spline_degree  = 3
    ),
    sampler_config = list(
      chains          = 4,
      iter_warmup     = 500,
      iter_sampling   = 500,
      max_treedepth   = as.integer(fit$metadata()$max_treedepth),
      adapt_delta     = fit$metadata()$adapt_delta
    )
  ),
  diagnostics = list(
    num_divergent    = sum(diag_summary$num_divergent),
    max_treedepth_hits = sum(diag_summary$num_max_treedepth),
    ebfmi            = as.list(diag_summary$ebfmi)
  ),
  parameters = list(
    tau = lapply(1:nrow(tau_summary), function(i) {
      row <- tau_summary[i, ]
      list(variable = row$variable, mean = row$mean, median = row$median,
           sd = row$sd, rhat = row$rhat, ess_bulk = row$ess_bulk)
    }),
    phi = list(
      mean = phi_summary$mean, median = phi_summary$median,
      sd = phi_summary$sd, rhat = phi_summary$rhat,
      ess_bulk = phi_summary$ess_bulk
    )
  ),
  checks = list(
    election_recovery = list(
      overall = list(
        coverage_90 = election_coverage$coverage_90,
        coverage_50 = election_coverage$coverage_50,
        mae         = election_coverage$mae
      ),
      by_party = lapply(1:nrow(election_by_party), function(i) {
        row <- election_by_party[i, ]
        list(party = as.character(row$party), coverage_90 = row$coverage_90,
             coverage_50 = row$coverage_50, mae = row$mae)
      }),
      by_election = lapply(1:nrow(election_df), function(i) {
        row <- election_df[i, ]
        list(election = as.character(row$election_date), party = as.character(row$party),
             actual = row$actual, median = row$median,
             q05 = row$q05, q95 = row$q95,
             covered_90 = row$covered_90)
      })
    ),
    residuals = list(
      by_party = lapply(1:nrow(residual_by_party), function(i) {
        row <- residual_by_party[i, ]
        list(party = as.character(row$party), mean_residual = row$mean_residual,
             sd_residual = row$sd_residual, rmse = row$rmse)
      })
    ),
    ppc = list(
      n_observations = n_ppc,
      n_replications = n_rep,
      coverage_90_by_party = lapply(1:nrow(ppc_calibration), function(i) {
        row <- ppc_calibration[i, ]
        list(party = as.character(row$party), coverage_90 = row$coverage_90)
      })
    )
  )
)

## Save with timestamp
benchmark_file <- sprintf("data/benchmarks/spline_%s.json",
                          format(Sys.time(), "%Y%m%d_%H%M%S"))
write_json(benchmark, benchmark_file, pretty = TRUE, auto_unbox = TRUE)
message(sprintf("Saved: %s", benchmark_file))

message("\n=== Retrodictive checks complete ===")
