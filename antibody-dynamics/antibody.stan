data {
  int N;
  vector[N] y;
}

parameters {
  real mu;
  real<lower = 0> sigma;
  real<lower=0> lambda;
  real<lower=0,upper=1> p;
  real<lower=0> shift;
}

model {
  // priors
  
  mu ~ normal(-1.25, 10);
  sigma ~ normal(0.3,10);
  lambda ~ normal(10,10);
  shift ~ normal(1.16,10);
  
  // likelihood
  
  for(n in 1:N) {
    target += log_sum_exp(log(1-p) + normal_lpdf(y[n] | mu, sigma),
    log(p) + exp_mod_normal_lpdf(y[n] - shift | mu, sigma, lambda));
  }
}
