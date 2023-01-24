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
  real mu;
  real a[2];
  real x[N];
}

transformed parameters {
   //real sig1 = sig*sqrt(1-a[2])/((1+a[2])*(1-a[1]-a[2])*(1+a[1]-a[2]));
   real maxa2 = fmin(1-a[1],1+a[1]); 
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  a[1] ~ uniform(-1,1);
  a[2] ~ uniform(-1,maxa2);
  mu ~ normal(0,10);
  x[1] ~ normal(mu,sig);
  y[1] ~ bernoulli_logit(x[1]);
  x[2] ~ normal(mu,sig);
  y[2] ~ bernoulli_logit(x[2]);
  for(i in 2:N)
  {
    x[i] ~ normal(mu+a[1]*(x[i-1]-mu)+a[2]*(x[2]-mu),sig);
    y[i] ~ bernoulli_logit(x[i]);
   }
}

