// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] sales;
  vector[N] temperature;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real Intercept;
  real beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    sales ~ normal(Intercept+beta*temperature, sigma);
}
