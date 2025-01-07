/*
Model Structure:

1. Initial State (t ≤ t_transition):
   - K+1 categories total (K parties plus residual)
   - First party is baseline (logit = 0)
   - State vector η_t ∈ ℝᴷ represents logits relative to first party
   
   Random Walk on Logit Scale:
   η_t = η_{t-1} + ε_t
   where ε_t = L_pre_scaled * z_t, z_t ∼ N(0, I_K)
   
   L_pre_scaled = diag(σ_pre) * L_base
   where:
   - L_base is K×K lower triangular (Cholesky factor of correlation matrix)
   - σ_pre ∈ ℝ_+ᴷ are scale parameters
   
   Vote shares via softmax:
   p_t = softmax([0; η_t]) ∈ Δᴷ⁺¹
   where Δᴷ⁺¹ is the K+1 dimensional simplex

2. Post-Transition State (t > t_transition):
   - K+2 categories (K parties plus new party plus residual)
   - First party remains baseline
   - State vector η_t ∈ ℝᴷ⁺¹
   
   L_post_scaled construction (preserving correlation structure):
   [L_post_scaled]_{1:K,1:K} = L_pre_scaled
   [L_post_scaled]_{K+1,1:K} = σ_{post,K+1} * γ
   [L_post_scaled]_{K+1,K+1} = σ_{post,K+1} * √(1 - ∑γᵢ²)
   
   where:
   - γ ∈ [-1,1]ᴷ controls correlations of new party with existing parties
   - √(1 - ∑γᵢ²) ensures valid correlation structure
   
   Random Walk:
   η_t = η_{t-1} + ε_t
   where ε_t = L_post_scaled * z_t, z_t ∼ N(0, I_{K+1})

3. Observation Model:
   Pre-transition:  y_t ∼ Multinomial(N_pre, p_t)
   Post-transition: y_t ∼ Multinomial(N_post, p_t)

4. Key Parameters:
   - L_base: Cholesky factor of base correlation structure
   - γ: Correlation parameters for new party
   - λ: Inheritance parameter for transition
   - σ_pre, σ_post: Scale parameters for random walks

5. Latent Share Recovery:
   For t ≤ t_transition:
   N_latent[t] = λ * p_t[K+1]
   where p_t[K+1] is the residual category share
*/

data {
  int<lower=1> K;              // number of parties (excluding residual)
  int<lower=1> T;              // total time periods
  int<lower=1> t_transition;   // time period when new party emerges
  real<lower=1> N_pre;          // number of respondents pre-transition
  real<lower=1> N_post;         // number of respondents post-transition
  array[t_transition, K+1] int Y_pre;  // observations before transition (K parties + residual)
  array[T-t_transition, K+2] int Y_post; // observations after transition (K parties + new + residual)
  real<lower=0> scale_prior;   // prior scale for random walk variance
}
parameters {
  // Base correlation structure through Cholesky
  cholesky_factor_corr[K] L_base;    
  
  // Correlations for new party
  vector<lower=-1,upper=1>[K] gamma;
  
  // Scale parameters
  vector<lower=0>[K] sigma_pre;      
  real<lower=0> sigma_post;
  
  // Initial states
  vector[K] state_pre_init;

  // Non-centered parameterization: standardized innovations
  array[t_transition] vector[K] z_pre;      // Pre-transition innovations
  array[T-t_transition] vector[K+1] z_post;   // Post-transition innovations
  
  real<lower=0,upper=1> lambda;      // Inheritance parameter
}

transformed parameters {
  // Scale the base Cholesky factor for pre-transition
  matrix[K,K] L_pre_scaled = diag_pre_multiply(sigma_pre, L_base);
  
  // Construct post-transition scaled Cholesky
  matrix[K+1,K+1] L_post_scaled = rep_matrix(0, K+1, K+1);  // Initialize with zeros
  L_post_scaled[1:K, 1:K] = L_pre_scaled;
  L_post_scaled[K+1,1:K] = sigma_post * gamma';
  L_post_scaled[K+1,K+1] = sigma_post * sqrt(1 - sum(gamma .* gamma));
  
  // States constructed from innovations
  array[t_transition] vector[K + 1] state_pre;
  array[T - t_transition] vector[K + 2] state_post;
  
  // Initialize states
  state_pre[,1] = rep_array(0.0, t_transition);
  state_pre[1, 2:K + 1] = state_pre_init;
  
  // Construct states through non-centered evolution
  for (t in 2:t_transition) {
    state_pre[t, 2:K + 1] = state_pre[t-1, 2:K + 1] + L_pre_scaled * z_pre[t-1];
  }
  // 
  // Initialize post-transition state on logit scale
  {
    real residual_prob = inv_logit(state_pre[t_transition, K + 1]);
    vector[K + 2] tmp;
    tmp[1:K] = state_pre[t_transition, 1:K];
    tmp[K + 1] = logit(lambda * residual_prob);
    tmp[K + 2] = logit((1 - lambda) * residual_prob);
    state_post[1, 2:K + 2] = tmp[2:K+2] + L_post_scaled * z_post[1];
  }
  
  state_post[, 1] = rep_array(0.0, T - t_transition);
  // Post-transition evolution
  for (t in (2):T - t_transition) {
    state_post[t, 2:K + 2] = state_post[t-1, 2:K + 2] + L_post_scaled * z_post[t];
  }
  
  // Probability vectors (elements of simplices)
  array[t_transition]     vector[K+1] pi_pre;    // ∈ Δᴷ⁺¹
  array[T - t_transition] vector[K+2] pi_post;   // ∈ Δᴷ⁺²
  
  // Transform to probabilities with numerically stable softmax
  for (t in 1:t_transition) {
    pi_pre[t] = softmax(state_pre[t]);
  }
  for (t in 1:T - t_transition){
    pi_post[t] = softmax(state_post[t]);
  }
}

model {
  // Priors
  L_base ~ lkj_corr_cholesky(1);
  gamma ~ normal(0, 0.5);
  lambda ~ beta(10, 10);
  
  // Hierarchical priors on scales
  sigma_pre ~ normal(0, scale_prior);
  sigma_post ~ normal(0, scale_prior);
  
  // Initial state priors
  state_pre_init ~ normal(0, 0.5);

  // Standard normal priors on innovations (non-centered parameterization)
  for (t in 1:(t_transition)) {
    z_pre[t] ~ std_normal();
  }
  // 
  for (t in 1:(T-t_transition)) {
    z_post[t] ~ std_normal();
  }
  
  // Observation model
  for (i in 1:K + 1){
    Y_pre[,i] ~ poisson(to_vector(pi_pre[, i]) * N_pre);
    Y_post[,i] ~ poisson(to_vector(pi_post[, i]) * N_post);
  }
}
// 
generated quantities {
  matrix[K, K] Omega_pre = L_pre_scaled * L_pre_scaled';
  matrix[K + 1, K + 1] Omega_post = L_post_scaled * L_post_scaled';
}
