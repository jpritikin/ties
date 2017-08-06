// http://mc-stan.org/users/documentation/case-studies/rstan_workflow.html
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
  matrix[NPA,NFACETS]     rawTheta;    // latent score of PA by facet
  real threshold1;
  real threshold2;
  vector<lower=0>[NFACETS] alpha;
  vector[NPA] rawFlow;
  vector[NFACETS] rawLoadings;
}
model {
  rawFlow ~ normal(0, 1);
  rawLoadings ~ normal(0,5);
  for (pa in 1:NPA) {
    rawTheta[pa,] ~ normal(rawFlow[pa] * rawLoadings, 1);
  }
  threshold1 ~ normal(0,5);
  threshold2 ~ normal(0,5);
  alpha ~ lognormal(1, 1);
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      if (rcat[cmp,ff] == 13) continue;  // special value to indicate missing
      rcat[cmp,ff] ~ categorical_logit(
        cmp_probs(alpha[ff],
          rawTheta[pa1[cmp],ff],
          rawTheta[pa2[cmp],ff],
          threshold1, threshold2));
    }
  }
}
// add posterior predictive check TODO
generated quantities {
  vector[NFACETS] flowLoadings = rawLoadings;
  vector[NPA] flow = rawFlow;
  matrix[NPA,NFACETS] theta = rawTheta;

  if (flowLoadings[1] < 0) {
    flowLoadings = -flowLoadings;
    flow = -flow;
  }

  /*int rcat_sim[NCMP,NFACETS];
  for (cmp in 1:NCMP) {
    for (ff in 1:NFACETS) {
      rcat_sim[cmp,ff] = categorical_logit_rng(
        cmp_probs(alpha,
          theta[pa1[cmp],ff],
          theta[pa2[cmp],ff],
          threshold1, threshold2));
    }
    }*/
}
