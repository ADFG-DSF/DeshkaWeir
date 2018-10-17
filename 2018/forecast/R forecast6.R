library(preseason)
dat <- prep_brood(deshka, 4:6)

#sibling model
sib <- lm(age6_ln ~ age5_ln + age4_ln, data = dat)
summary(sib)
sib <- update(sib, formula. = . ~ age5_ln)
par(mfrow = c(2,2)); plot(sib); par(mfrow = c(1,1))
ggplot2::ggplot(dat, ggplot2::aes(x = age5_ln, y = age6_ln)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method=lm, se=TRUE)
forecast::tsdisplay(residuals(sib))
forecast::auto.arima(dat$age6_ln, xreg = dat$age5_ln)
sib_arima <- arima(dat$age6_ln, c(1,1,0), xreg = dat$age5_ln)

temp <- pred_arima(sib_arima, x = dat$age6_ln, xreg = dat$age5_ln)
dat$sib_pred <- exp(temp[1,] + temp[2,]^2/2)

#ricker
rick <- lm(lnRS ~ S, data = dat)
summary(rick)
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))
plot(dat$S, dat$lnRS)

dat$mu5_pred <- pred_ma(dat$age6_ln, yrs = 5)

forecast::tsdisplay(dat$age6_ln)
forecast::auto.arima(dat$age6_ln)
mu_ar1 <- arima(dat$age6_ln, order=c(1,1,0))
temp <- pred_arima(mu_ar1, x = dat$age6_ln)
dat$muarima_pred <- exp(temp[1,] + temp[2,]^2/2)

forecast::ets(dat$age6_ln, "ANN")
dat$es_pred <- pred_es(dat$age6_ln)

comp_models(dat, 6)
