functions {
  vector cmp_probs(real alpha, real pa1, real pa2, real thr1, real thr2) {
    vector[5] unsummed;
    real paDiff = (pa1 - pa2);
    unsummed[1] = 0;
    unsummed[2] = paDiff - (thr1 + thr2);
    unsummed[3] = paDiff - thr1;
    unsummed[4] = paDiff + thr1;
    unsummed[5] = paDiff + thr1 + thr2;
    return cumulative_sum(alpha * unsummed);
  }
}
data {
  // dimensions
  int<lower=1> NPA;             // # physical activites
  int<lower=1> NFACETS;         // # facets (i.e. characteristics)
  int<lower=1> NCMP;            // # comparisons (sample size)
  // actual data follows
  int<lower=1, upper=NPA> pa1[NCMP];        // PA1 for observation N
  int<lower=1, upper=3> l1[NCMP];           // L1 for observation N
  int<lower=1, upper=NPA> pa2[NCMP];        // PA2 for observation N
  int<lower=1, upper=3> l2[NCMP];           // L2 for observation N
  int<lower=-2, upper=10> diff[NCMP,NFACETS];   // comparisons
  int<lower=0> NSGP;
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
  matrix[NPA,NFACETS]     theta;    // latent score of PA by facet
  real threshold1;
  real threshold2;
  vector<lower=0>[NFACETS] alpha;
  cholesky_factor_corr[NFACETS] thetaCorChol;
}
model {
  thetaCorChol ~ lkj_corr_cholesky(2.0);
  for (pa in 1:NPA) {
    theta[pa,] ~ multi_normal_cholesky(rep_vector(0, NFACETS), thetaCorChol);
  }
  threshold1 ~ normal(0,5);
  threshold2 ~ normal(0,5);
  alpha ~ lognormal(0, 1);
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      if (rcat[cmp,ff] == 13) continue;  // special value to indicate missing
      rcat[cmp,ff] ~ categorical_logit(
        cmp_probs(alpha[ff],
          theta[pa1[cmp],ff],
          theta[pa2[cmp],ff],
          threshold1, threshold2));
    }
  }
}
generated quantities {
          corr_matrix[NFACETS] thetaCor;
          thetaCor = thetaCorChol * thetaCorChol'; // multiply_lower_tri_self_transpose(L);
}
