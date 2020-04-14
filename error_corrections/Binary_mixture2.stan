// Binary Mixture model with two different sd's
 
data {
  int<lower=1>  n;
  real          logod[n];
}

// Now use two sd's
parameters{
  real          mu_baseline;
  real<lower=0> delta;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  real<lower=0, upper=1> p;
}


model { 
  mu_baseline ~ normal( 1, 2 );
  delta ~ normal(  2, 1 );
  sigma1 ~ normal( 0.5, 0.5 );
  sigma2 ~ normal( 0.5, 0.5 );
  p ~ beta(2,2);
  
  for( j in 1:n ){
      target += log_sum_exp(log(1-p) + normal_lpdf(logod[j] | mu_baseline, sigma1),
                           log(p)    + normal_lpdf(logod[j] | mu_baseline+delta, sigma2));
  }
}


generated quantities{
  real logod_corr;
  real sick;
  sick = bernoulli_rng(p);
  if (sick==1){
    logod_corr = normal_rng(mu_baseline+delta,sigma2);
  }
  else{
    logod_corr = normal_rng(mu_baseline,sigma1);
  }
}

