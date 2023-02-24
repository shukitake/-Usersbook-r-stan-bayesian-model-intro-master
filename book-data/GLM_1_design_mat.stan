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
  int K;
  int Y[N];
  matrix[N,K] X;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[K] b;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[N] lambda=exp(X*b);
  Y ~ poisson(lambda);
}
generated quantities {
vector[N] log_lik;
for (n in 1:N)
  log_lik[n] = poisson_lpmf(Y[n]|exp(X[n,:]*b));
}
