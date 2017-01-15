functions {
  vector cmp_probs(real alpha, real pa1, real pa2, real thr1, real thr2) {
    vector[5] unsummed;
    real paDiff = pa1 - pa2;
    unsummed[1] = 0;
    unsummed[2] = paDiff - thr2;
    unsummed[3] = paDiff - thr1;
    unsummed[4] = paDiff + thr1;
    unsummed[5] = paDiff + thr2;
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
  matrix[NPA,NFACETS]     theta[3];    // latent score of PA by facet
  real<lower=0> threshold1;
  real<lower=0> threshold2;
  vector<lower=0>[NFACETS] alpha;
  real<lower=1e-15>  betweenLevelVariance;
//  real<lower=1e-15> betweenSoloGroupVariance;
}
model {
  betweenLevelVariance ~ exponential(.01);
  for (lev in 1:2) {
    for (pa in 1:NPA) {
      // estimate a separate variance parameter for each pair of levels TODO
      for (facet in 1:NFACETS) {
        theta[lev,pa,facet] ~ normal(theta[lev+1,pa,facet], sqrt(betweenLevelVariance));
      }
    }
  }
  // betweenSoloGroupVariance ~ exponential(.01);
  // for (lev in 1:3) {
  //   for (px in 1:NSGP) {
  //     int p1 = soloGroupList[px]-1;
  //     int p2 = soloGroupList[px];
  //     for (facet in 1:NFACETS) {
  //       theta[lev,p1,facet] ~ normal(theta[lev,p2,facet], betweenSoloGroupVariance);
  //     }
  //   }
  // }
  for (lev in 1:3) {
    for (pa in 1:NPA) {
      theta[lev,pa,1:NFACETS] ~ normal(0, 1);  //sd=thetaScale?
    }
  }
  threshold1 ~ normal(0,5);
  threshold2 ~ normal(0,5);
  alpha ~ lognormal(1, 1);
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      if (rcat[cmp,ff] == 13) continue;  // special value to indicate missing
      rcat[cmp,ff] ~ categorical_logit(
        cmp_probs(alpha[ff],
          theta[l1[cmp],pa1[cmp],ff],
          theta[l2[cmp],pa2[cmp],ff],
          threshold1, threshold2));
    }
  }
}
