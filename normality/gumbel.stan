data {
  int N;
  vector[N] y;
  int n_groups;
}

parameters {
  ordered[n_groups] mu;
  vector<lower = 0>[n_groups] sigma;
  simplex[n_groups] Theta;
}

model {
  vector[n_groups] contributions;
  
  // priors
  
  mu ~ normal(0, 10);
  sigma ~ lognormal(0,2);
  Theta ~ dirichlet(rep_vector(2.0, n_groups));
  
  // likelihood
  
  for(i in 1:N) {
    for(k in 1:n_groups) {
      contributions[k] = log(Theta[k]) + gumbel_lpdf(-y[i] | mu[k], sigma[k]);
    }
    target += log_sum_exp(contributions);
  }
}
