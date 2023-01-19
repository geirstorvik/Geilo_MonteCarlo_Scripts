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
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  real<lower=0> sig;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real a;
  real x[N];
}

transformed parameters {
   real sig1 = sig/sqrt(1-a^2);
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  a ~ uniform(0,1);
  x[1] ~ normal(0.0,sig1);
  y[1] ~ bernoulli_logit(x[1]);
  for(i in 2:N)
  {
    x[i] ~ normal(a*x[i-1],sig);
    y[i] ~ bernoulli_logit(x[i]);
   }
}

