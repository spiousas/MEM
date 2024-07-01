data {
  int<lower=2> K;
  int<lower=0> N;
  vector[N] x;
  array[N] int<lower=1, upper=K> y;
}

parameters {
  real beta;
  ordered[K - 1] c;
}


model {
  for (n in 1:N) {
    y[n] ~ ordered_logistic(x[n] * beta, c);
  }
  beta ~ normal(0,10);
}


// model {
//   vector[K] theta;
//   for (n in 1:N) {
//     real eta;
//     eta = x[n] * beta;
//     theta[1] = 1 - Phi(eta - c[1]);
//     for (k in 2:(K - 1)) {
//       theta[k] = Phi(eta - c[k - 1]) - Phi(eta - c[k]);
//     }
//     theta[K] = Phi(eta - c[K - 1]);
//     y[n] ~ categorical(theta);
//   }
//   beta ~ normal(0,10);
// }
