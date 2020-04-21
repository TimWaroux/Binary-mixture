//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

data { // input data
  int<lower=0> n;          // Number of data points
  int<lower=0> ctrls_neg;
  int<lower=0> ctrls_pos;
  vector[n] logod;         // log-od values
  int<lower=1> plate_id[n]; // plate id restarts at 1 each day
  int<lower=1> day_id[n]; // day id
}


transformed data {
  int<lower=1> n_plates_per_day;
  //vector[n_days] n_plates_per_day; // vector
  int<lower=1> n_days;

  vector[ctrls_neg] logod_ctrl_neg;     // logod of negative controls
  vector[ctrls_pos] logod_ctrl_pos;     // logod of positive controls

  n_plates_per_day = max(plate_id);
  n_days = max(day_id);


// First data points are controls
for (i in 1:ctrls_neg){
  logod_ctrl_neg[i] = logod[i];
}

for (i in 1:ctrls_pos){
  logod_ctrl_pos[i] = logod[i+ctrls_neg];
}

}




parameters {
  
  real mu_baseline;
  real<lower=0> delta;
  real<lower=0, upper=1> p;
  real<lower=0> sigma_baseline;
  real<lower=0> sigma_plate;

  // for the day errors
  simplex[n_days] alpha_raw;  // simplex so sum = 1
  real alpha_scale;
  
  // make beta_raw simplex and beta_scale for the plate errors of each day
  real beta_scale[n_days];
  simplex[n_plates_per_day] beta_raw[n_days];
}


transformed parameters {
  vector[n] mu_hat_plate; // mean of plate = mean of plate's day + plate error
  vector[n_days] alpha; // error of each day
  matrix[n_days, n_plates_per_day] beta; // will contain the beta errors of the plates per day (row)
  matrix[n_days, n_plates_per_day] alpha_matrix; // each row = n_platers_per_day long vector of identical alpha_i for summation of beta
  
  
  alpha = alpha_scale*(alpha_raw-inv(n_days));
  for(d in 1:n_days){
    // make alpha_matrix:
    alpha_matrix[d] = rep_row_vector(alpha[d], n_plates_per_day);
    // Keep beta centered:
    beta[d] = to_row_vector(beta_scale[d]*(beta_raw[d]-inv(n_plates_per_day))) + alpha[d];
    
    }
  
  
  for(k in 1:n){
   mu_hat_plate[k] = mu_baseline + beta[day_id[k], plate_id[k]];
  }
}



model {
  mu_baseline ~ normal( -1, 2 ); // prior distribution of mu_baseline
  sigma_baseline ~normal( 0.8, 0.5 );
  sigma_plate ~ normal(0,0.2);

  alpha_scale ~ normal( 0, 1 );
  alpha_raw ~ normal( 1, 1 );
  for(d in 1:n_days){
    beta_scale[d] ~ normal( 0, 1 ); // prior distributions of scale vars
    beta_raw[d,] ~ normal( 1, 1 );
  } 
  delta ~ normal(2, 1);
  p ~ beta(2,2);
  
  
  
  logod_ctrl_neg ~ normal( mu_hat_plate[1:ctrls_neg],
                          sqrt(square(sigma_baseline)+square(sigma_plate)));
  logod_ctrl_pos ~ normal( mu_hat_plate[ctrls_neg+1:ctrls_neg+ctrls_pos]+delta,
                          sqrt(square(sigma_baseline)+square(sigma_plate)));

  for (j in ctrls_neg+ctrls_pos+1:n){
    target += log_sum_exp(log(1-p) + normal_lpdf(logod[j] | mu_hat_plate[j],
                          sqrt(square(sigma_baseline)+square(sigma_plate))),
                          log(p) + normal_lpdf(logod[j] | mu_hat_plate[j]+delta,
                          sqrt(square(sigma_baseline)+square(sigma_plate))));
  }

}



generated quantities{
  real logod_corr;
  real sick;
  sick = bernoulli_rng(p);
  if (sick==1){
    logod_corr = normal_rng(mu_baseline+delta,sigma_baseline);
  }
  else{
    logod_corr = normal_rng(mu_baseline,sigma_baseline);
  }
  
}
