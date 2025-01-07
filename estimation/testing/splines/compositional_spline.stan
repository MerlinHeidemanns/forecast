
data {
  int<lower=1> N;          // number of observations
  int<lower=1> K;          // number of basis functions
  int<lower=2> P;          // number of parties
  vector[N] x;             // time
  array[N, P] int y;            // vote counts for P parties
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

