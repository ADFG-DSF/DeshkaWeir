library(preseason)
dat <- prep_brood(deshka, 3:4)

#no sibling relationship
plot(dat$age3_ln, dat$age4_ln)

#ricker
rick <- lm(lnRS ~ S, data = dat)
summary(rick)
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))
plot(dat$S, dat$lnRS)

forecast::tsdisplay(residuals(rick))
forecast::auto.arima(rick$model$lnRS, xreg = rick$model$S)
rick_ts <- arima(dat$age4_ln, order=c(1,0,0), method = "ML")
AIC(rick, rick_ts)
dat$ricker_pred <- exp(pred_arima(rick_ts, dat$lnRS, dat$S)[1, ]) * dat$S

dat$mu5_pred <- pred_ma(dat$age4_ln, yrs = 5)

forecast::tsdisplay(dat$age4_ln)
forecast::auto.arima(dat$age4_ln)
mu_ts <- arima(dat$age4_ln, order=c(1,0,0))
temp <- pred_arima(mu_ts, x = dat$age4_ln)
dat$mu_pred <- exp(temp[1,] + temp[2,]^2/2)

forecast::ets(dat$age4_ln, "ANN")
dat$es_pred <- pred_es(dat$age4_ln)

comp_models(dat, 4)
