data {
  int<lower=1> MANIFESTS;
  int<lower=1> NROW;
  vector[MANIFESTS] DATA[NROW];
}
parameters {
  vector[MANIFESTS] means;
  real rawLatent[NROW];
  vector[MANIFESTS] rawLoadings;
  vector<lower=0>[MANIFESTS] rload;
}
transformed parameters {
  vector[MANIFESTS] perRowMean[NROW];
  cov_matrix[MANIFESTS] cov1;
  cholesky_factor_cov[MANIFESTS] cf;
  
  for (rx in 1:NROW) {
    perRowMean[rx] = means + rawLatent[rx] * rawLoadings;
  }
  // only need to add rawLoadings * rawLoadings' to cov1 for a marginal likelihood
  cov1 = diag_matrix(rload .* rload);
  cf = cholesky_decompose(cov1);
}
model {
  rawLoadings ~ normal(0,10);
  rload ~ normal(0,1);
  means ~ normal(0,10);
  rawLatent ~ normal(0,1);
  DATA ~ multi_normal_cholesky(perRowMean, cf);
}
generated quantities {
  vector[MANIFESTS] loadings = rawLoadings;
  vector[MANIFESTS] resid;
  resid = rload .* rload;

  if (loadings[1] < 0) loadings = -loadings;
}
