data {
  int<lower=1> MANIFESTS;
  int<lower=1> NROW;
  vector[MANIFESTS] DATA[NROW];
}
parameters {
  vector[MANIFESTS] means;
  real latent[NROW];
  real<lower=0> loadings1;
  vector[MANIFESTS-1] loadingsX;
  vector<lower=0>[MANIFESTS] rload;
}
transformed parameters {
  vector[MANIFESTS] perRowMean[NROW];
  cov_matrix[MANIFESTS] cov1;
  cholesky_factor_cov[MANIFESTS] cf;
  vector[MANIFESTS] loadings;
  
  loadings[1] = loadings1;
  for (lx in 2:MANIFESTS) {
    loadings[lx] = loadingsX[lx-1];
  }

  for (rx in 1:NROW) {
    perRowMean[rx] = means + latent[rx] * loadings;
  }
  // only need to add loadings * loadings' for a marginal likelihood
  cov1 = diag_matrix(rload .* rload);
  cf = cholesky_decompose(cov1);
}
model {
  loadings1 ~ normal(0,10);
  loadingsX ~ normal(0,10);
  rload ~ normal(0,1);
  means ~ normal(0,10);
  latent ~ normal(0,1);
  DATA ~ multi_normal_cholesky(perRowMean, cf);
}
generated quantities {
  vector[MANIFESTS] resid;
  resid = rload .* rload;
}
