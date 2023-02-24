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
  int N_s;
  int K;
  int Y[N];
  matrix[N,K] X;
  int shop[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[K] b;
  vector[N_s] r;
  real sigma_r;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
transformed parameters{
  vector[N] lambda = exp(X*b+r[shop]); 
}
model {
  for (i in 1:N_s)
    r[i] ~ normal(0, sigma_r);
  Y~ poisson(lambda);
}
