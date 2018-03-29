// single factor model with single set of thresholds
functions {
  vector cmp_probs(real alpha, real pa1, real pa2, real thr1, real thr2) {
    vector[5] unsummed;
    real paDiff = alpha * (pa1 - pa2);
    unsummed[1] = 0;
    unsummed[2] = paDiff - (thr1 + thr2);
    unsummed[3] = paDiff - thr1;
    unsummed[4] = paDiff + thr1;
    unsummed[5] = paDiff + thr1 + thr2;
    return cumulative_sum(unsummed);
  }
}
data {
  // dimensions
  int<lower=1> NPA;             // number of physical activites
  int<lower=1> NFACETS;         // number of facets (i.e. characteristics)
  int<lower=1> NCMP;            // number of comparisons (sample size)
  int<lower=1> N;               // number of non-missing data
  // actual data follows
  int<lower=1, upper=NPA> pa1[NCMP];        // PA1 for observation N
  int<lower=1, upper=NPA> pa2[NCMP];        // PA2 for observation N
  int<lower=-2, upper=10> diff[NCMP,NFACETS];   // comparisons
  //  int<lower=0> NSGP;
//  int<lower=1> soloGroupList[NSGP];
}
transformed data {
  int rcat[NCMP,NFACETS];

  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      rcat[cmp,ff] = diff[cmp,ff] + 3;
    }
  }
}
parameters {
  matrix[NPA,NFACETS]     theta_raw;
  real threshold1;
  real threshold2;
  row_vector<lower=0>[NFACETS] sigma;
  vector[NPA] rawFlow;
  vector[NFACETS] rawLoadings;
}
transformed parameters {
  real alpha = mean(sigma .* sigma)^3.0;
  // non-centered parameterization due to thin data
  matrix[NPA,NFACETS]     theta;    // latent score of PA by facet
  for (pa in 1:NPA) {
    theta[pa,] = sigma .* ((rawFlow[pa] * rawLoadings)' + theta_raw[pa,]);
  }
}
model {
  rawFlow ~ normal(0, 1);
  rawLoadings ~ normal(0,5);
  for (pa in 1:NPA) {
    theta_raw[pa,] ~ normal(0, 1);
  }
  threshold1 ~ normal(0,5);
  threshold2 ~ normal(0,5);
  sigma ~ lognormal(0, 1);
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      if (rcat[cmp,ff] == 13) continue;  // special value to indicate missing
      rcat[cmp,ff] ~ categorical_logit(cmp_probs(alpha, theta[pa1[cmp],ff],
                                                 theta[pa2[cmp],ff],
                                                 threshold1, threshold2));
    }
  }
}
generated quantities {
  vector[N] log_lik;
  vector[NFACETS] flowLoadings = rawLoadings;
  vector[NPA] flow = rawFlow;
  int rcat_sim[NCMP,NFACETS];
  int cur = 1;

  if (flowLoadings[1] < 0) {
    flowLoadings = -flowLoadings;
    flow = -flow;
  }

  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      rcat_sim[cmp,ff] = categorical_logit_rng(cmp_probs(alpha, theta[pa1[cmp],ff],
                                                         theta[pa2[cmp],ff],
                                                         threshold1, threshold2)) - 3;
    }
  }

  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      if (rcat[cmp,ff] == 13) continue;  // special value to indicate missing
      log_lik[cur] = categorical_logit_lpmf(rcat[cmp,ff] |
                                            cmp_probs(alpha, theta[pa1[cmp],ff],
                                                      theta[pa2[cmp],ff],
                                                      threshold1, threshold2));
      cur = cur + 1;
    }
  }
}
