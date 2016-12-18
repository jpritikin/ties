functions {
  vector cmp_probs(real pa1, real pa2, real thr1, real thr2) {
    vector[5] unsummed;
    real diff = pa1 - pa2;
    unsummed[1] = 0;
    unsummed[2] = diff - thr2;
    unsummed[3] = diff - thr1;
    unsummed[4] = diff + thr1;
    unsummed[5] = diff + thr2;
    return cumulative_sum(unsummed);
  }
}
data {
  // dimensions
  int<lower=1> NPA;             // # physical activites
  int<lower=1> NFACETS;         // # facets (i.e. characteristics)
  int<lower=1> NCMP;            // # comparisons (sample size)
  // actual data follows
  int<lower=1, upper=NPA> pa1[NCMP];        // PA1 for observation N
  int<lower=1, upper=NPA> pa2[NCMP];        // PA2 for observation N
  int<lower=-2, upper=2> diff[NCMP,NFACETS];   // comparisons
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
  matrix[NPA,NFACETS]     theta;    // latent score of PA by facet
  real<lower=0> threshold1;
  real<lower=0> threshold2;
  // add per facet slopes TODO
}
model {
//  threshold1 ~ normal(0,5);
//  threshold1 ~ normal(0,5);
  for (pa in 1:NPA) {
    theta[pa,1:NFACETS] ~ normal(0, 1);
  }
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      rcat[cmp,ff] ~ categorical_logit(
        cmp_probs(
          theta[pa1[cmp],ff],
          theta[pa2[cmp],ff],
          threshold1, threshold2));
    }
  }
}
