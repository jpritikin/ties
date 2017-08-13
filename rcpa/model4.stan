// http://mc-stan.org/users/documentation/case-studies/rstan_workflow.html
functions {
  vector cmp_probs(real alpha, real pa1, real pa2, real thr1, real thr2) {
    vector[5] unsummed;
    real paDiff = pa1 - pa2;
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
  int<lower=1> NCMP;            // # comparisons (sample size)
  // actual data follows
  int<lower=1, upper=NPA> pa1[NCMP];        // PA1 for observation N
  //  int<lower=1, upper=3> l1[NCMP];           // L1 for observation N
  int<lower=1, upper=NPA> pa2[NCMP];        // PA2 for observation N
  //  int<lower=1, upper=3> l2[NCMP];           // L2 for observation N
  int<lower=-2, upper=10> diff[NCMP];   // comparisons
  //  int<lower=0> NSGP;
//  int<lower=1> soloGroupList[NSGP];
}
transformed data {
  int rcat[NCMP];

  for (cmp in 1:NCMP) {
    rcat[cmp] = diff[cmp] + 3;
  }
}
parameters {
  real threshold1;
  real threshold2;
  //  vector<lower=0>[NFACETS] alpha;
  real<lower=0> alpha;
  real flow[NPA];
}
model {
  flow ~ normal(0,1);
  threshold1 ~ normal(0,5);
  threshold2 ~ normal(0,5);
  alpha ~ lognormal(1, 1);
  for (cmp in 1:NCMP) {
    if (rcat[cmp] == 13) continue;  // special value to indicate missing
    rcat[cmp] ~ categorical_logit(
        cmp_probs(alpha,
          flow[pa1[cmp]],
          flow[pa2[cmp]],
          threshold1, threshold2));
  }
}
// add posterior predictive check TODO
generated quantities {
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
