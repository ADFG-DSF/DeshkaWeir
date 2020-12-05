data {
  int N;
  int w[N];
  vector[N] day;
  vector[2] flood1;
  vector[2] flood2;
}

parameters {
  real<lower = 0, upper=N> rt_mu;
  real<lower = 0, upper = 30> rt_80;
  real<lower=6820, upper=14000> weir;
  real<lower=0.01> phi;
}

transformed parameters {
  real<lower=0, upper=1> p[N];
  real w_mu[N];
for(n in 1:N){
//	p[n] = logistic_cdf(day[n], rt_mu, rt_80 / 4.394449) - logistic_cdf(day[n] - 1, rt_mu, rt_80 / 4.394449);
	p[n] = cauchy_cdf(day[n], rt_mu, rt_80 / 6.15) - cauchy_cdf(day[n] - 1, rt_mu, rt_80 / 6.15);
	w_mu[n] = weir * p[n];
  }
}

model {
	w ~ neg_binomial_2(w_mu, phi);
}

generated quantities {
  real p_f1;
  real p_f2;
  real weir_f1;
  real weir_f2;
//  p_f1 = logistic_cdf(flood1[2], rt_mu, rt_80 / 4.394449) - logistic_cdf(flood1[1] - 1, rt_mu, rt_80 / 4.394449);
  p_f1 = cauchy_cdf(flood1[2], rt_mu, rt_80 / 6.15) - cauchy_cdf(flood1[1] - 1, rt_mu, rt_80 / 6.15);
  weir_f1 = weir * p_f1;
//  p_f2 = logistic_cdf(flood2[2], rt_mu, rt_80 / 4.394449) - logistic_cdf(flood2[1] - 1, rt_mu, rt_80 / 4.394449);
  p_f2 = cauchy_cdf(flood2[2], rt_mu, rt_80 / 6.15) - cauchy_cdf(flood2[1] - 1, rt_mu, rt_80 / 6.15);
  weir_f2 = weir * p_f2;
}
