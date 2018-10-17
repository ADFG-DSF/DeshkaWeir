library(preseason)
dat <- prep_brood(deshka, 3:4)

#ricker
rick <- lm(lnRS ~ S, data = dat)
summary(rick)
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))
plot(dat$S, dat$lnRS)

forecast::tsdisplay(residuals(rick))
forecast::auto.arima(rick$model$lnRS, xreg = rick$model$S)
dat$ricker_pred <- exp(pred_lm(rick)[1,]) * rick$model$S

dat$mu5_pred <- pred_ma(dat$age4_ln, yrs = 5)

forecast::tsdisplay(dat$age4_ln)
forecast::auto.arima(dat$age4_ln)
mu_ma1 <- arima(dat$age4_ln, order=c(0,0,1))
temp <- pred_arima(mu_ma1, x = dat$age4_ln)
dat$muarima_pred <- exp(temp[1,] + temp[2,]^2/2)

forecast::ets(dat$age4_ln, "ANN")
dat$es_pred <- pred_es(dat$age4_ln)

comp_models(dat, 4)
