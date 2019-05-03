// saved as 8schools.stan
//real b[M, N];

data {
  int<lower=0> N;         // number of samples 
  int<lower=0> J;         // number of subjects
  real rts[J, N];            // reaction times
  real choices[J, N];        // choices 
  //real min_rt[J];            // minimum rt in samples
}

parameters {
  real<lower=0> a[J];                   // boundary separation
  real<lower=0> n_dt[J];                // non decision time
  real v[J];                            // drift
  real<lower=0, upper=1> w[J];          // starting bias
}

// transformed parameters {
  //   vector[J] theta = mu + tau * eta;        // school treatment effects
  // }

//     
  // target += normal_lpdf(eta | 0, 1);       // prior log-density
// target += normal_lpdf(y | theta, sigma); // log-likelihood

model {
  for (j in 1:J){
    a[j] ~ uniform(0, 10);
    //n_dt[j] ~ uniform(0, min_rt[j]);
    w[j] ~ uniform(0, 1);
    v[j] ~ normal(0, 100);
    for (n in 1:N){
      if(choices[j,n] < 0)
        rts[j,n] ~ wiener(a[j], 0.00001, 1 - w[j], -v[j]);
      else
        rts[j,n] ~ wiener(a[j], 0.00001, w[j], v[j]);
    }
  }
}
