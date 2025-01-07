data {
  int<lower=0> N_fixed;      // number of fixed points (10)
  int<lower=0> N_obs;        // number of observed points (40)
  int<lower=0> N;            // total number of points (N_fixed + N_obs)
  array[N_fixed] real x_fixed;   // input locations for fixed points
  vector[N_fixed] y_fixed;   // fixed output values
  array[N_obs] real x_obs;       // input locations for observations
  vector[N_obs] y_obs;       // observed output values
  real<lower=0> noise_scale; // observation noise scale
}

parameters {
  real<lower=0> rho;       // length scale
  real<lower=0> alpha;     // marginal standard deviation
  vector[N_obs] z;         // standard normal variables for GP
}

transformed parameters {
  vector[N_obs] f_obs;     // latent function values at observed points
  
  {
    // Combine input locations into array
    array[N] real x;
    x[1:N_fixed] = x_fixed;
    x[(N_fixed + 1):N] = x_obs;
    
    // Extract blocks and add diagonal terms for stability
    // K = [K_ff   K_of']  where K_ff = K[1:n,1:n]
    //     [K_of   K_oo ]        K_of = K[n+1:N,1:n]
    //                           K_oo = K[n+1:N,n+1:N]
    matrix[N, N] K = gp_exp_quad_cov(x, alpha, rho);
    matrix[N_fixed, N_fixed] K_ff = K[1:N_fixed, 1:N_fixed] + 
      diag_matrix(rep_vector(1e-9, N_fixed));
    matrix[N_obs, N_fixed] K_of = K[(N_fixed + 1):N, 1:N_fixed];
    matrix[N_obs, N_obs] K_oo = K[(N_fixed + 1):N, (N_fixed + 1):N] + 
      diag_matrix(rep_vector(1e-9, N_obs));
    
    // Cholesky decomposition K_ff = L_ff * L_ff'
    matrix[N_fixed, N_fixed] L_ff = cholesky_decompose(K_ff);
    
    // First part of solving K_ff^(-1) * y_fixed
    // L_ff^(-1) * y_fixed
    vector[N_fixed] L_inv_y = mdivide_left_tri_low(L_ff, y_fixed);
    
    // First part of solving K_ff^(-1) * K_of'
    // K_of is [N_obs, N_fixed], so we solve with K_of
    // to get L_ff^(-1) * K_of' which is [N_fixed, N_obs]
    matrix[N_fixed, N_obs] L_inv_Kof = mdivide_left_tri_low(L_ff, K_of');
    
    // Conditional mean: μ₂|₁ = K_of * K_ff^(-1) * y_fixed
    //                 = K_of * (L_ff * L_ff')^(-1) * y_fixed
    //                 = K_of * L_ff'^(-1) * L_ff^(-1) * y_fixed
    vector[N_obs] mu_cond = K_of * L_inv_y;
    
    // Conditional covariance: Σ₂|₁ = K_oo - K_of * K_ff^(-1) * K_of'
    //                              = K_oo - K_of * L_ff'^(-1) * L_ff^(-1) * K_of'
    // K_oo = [N_obs, N_obs]
    // K_of = [N_obs, N_fixed]
    
    matrix[N_obs, N_obs] K_cond = K_oo - K_of * L_inv_Kof;
    
    // Cholesky of conditional covariance for sampling
    // L_cond * L_cond' = K_cond
    matrix[N_obs, N_obs] L_cond = cholesky_decompose(add_diag(K_cond, rep_vector(1e-5, N_obs)));
    
    // Sample from conditional: f₂|f₁ ~ N(μ₂|₁, Σ₂|₁)
    // Equivalent to: μ₂|₁ + L_cond * z where z ~ N(0,I)
    f_obs = mu_cond + L_cond * z;
  }
}

model {
  // Priors
  rho ~ inv_gamma(5, 5);
  alpha ~ normal(0, 2);
  z ~ std_normal();  // prior for standardized GP variables
  
  // Likelihood
  y_obs ~ normal(f_obs, noise_scale);
}

generated quantities {
  vector[N] f_all = append_row(y_fixed, f_obs);
}