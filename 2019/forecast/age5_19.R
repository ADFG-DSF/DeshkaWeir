library(preseason)
dat <- prep_brood(deshka, 3:5)

#sibling model
ggplot2::ggplot(dat, ggplot2::aes(x = age4_ln, y = age5_ln, size = age3_ln)) + ggplot2::geom_point()
sib <- lm(age5_ln ~ age4_ln + age3_ln, data = dat)
summary(sib)
par(mfrow = c(2,2)); plot(sib); par(mfrow = c(1,1))
forecast::tsdisplay(residuals(sib))
temp <- pred_lm(sib)
dat$sib_pred <- exp(temp[1,] + temp[2,]^2/2)

#ricker
plot(dat$S, dat$lnRS)
rick <- lm(lnRS ~ S, data = dat)
summary(rick)
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))

forecast::tsdisplay(residuals(rick))
forecast::auto.arima(rick$model$lnRS, xreg = rick$model$S)
rick_ar1 <- arima(rick$model$lnRS, order=c(1,0,0), xreg = rick$model$S, method = "ML")
AIC(rick, rick_ar1)
rick_ar1
forecast::tsdisplay(residuals(rick_ar1))
dat$ricker_pred <- exp(pred_arima(rick_ar1, x = rick$model$lnRS, xreg = rick$model$S)[1,]) * rick$model$S

#Moving average
dat$mu5_pred <- pred_ma(dat$age5_ln, yrs = 5)

#mean
forecast::tsdisplay(dat$age5_ln)
forecast::auto.arima(dat$age5_ln)
mu_ts <- arima(dat$age5_ln, order=c(1,0,0))
temp <- pred_arima(mu_ts, x = dat$age5_ln)
dat$muarima_pred <- exp(temp[1,] + temp[2,]^2/2)

#Exponential smoothing
forecast::ets(dat$age5_ln, "ANN")
dat$es_pred <- pred_es(dat$age5_ln)

comp_models(dat, 5)
