data {
  int<lower=1> MANIFESTS;
  int<lower=1> NROW;
  vector[MANIFESTS] DATA[NROW];
}
parameters {
  vector[MANIFESTS] means;
  real latent[NROW];
  vector<lower=0>[MANIFESTS] loadings;
  vector<lower=0>[MANIFESTS] rload;
}
transformed parameters {
  cov_matrix[MANIFESTS] cov1;
  cholesky_factor_cov[MANIFESTS] cf;
  
  # only need to add loadings * loadings' for a marginal likelihood
  cov1 = diag_matrix(rload .* rload);
  cf = cholesky_decompose(cov1);
}
model {
  loadings ~ normal(0,100);
  rload ~ normal(0,1);
  means ~ normal(0,10);
  latent ~ normal(0,1);
  for (rx in 1:NROW) {
    DATA[rx] ~ multi_normal_cholesky(means + latent[rx] * loadings, cf);
  }
}
generated quantities {
  vector[MANIFESTS] resid;
  resid = rload .* rload;
}
