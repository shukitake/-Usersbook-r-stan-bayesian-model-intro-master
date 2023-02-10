//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int N;
  int FISH_NUM[N];
  vector[N] SUNNY;
  vector[N] TEMP;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real intercept;
  real b_TEMP;
  real b_SUNNY;
  vector[N] r;
  real<lower=0> sigma_r;
}
transformed parameters{
  vector[N] lambda = intercept + b_SUNNY*SUNNY +b_TEMP*TEMP + r;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  r ~ normal(0, sigma_r);
  FISH_NUM ~ poisson_log(lambda);
}

