library(preseason)
dat <- prep_brood(deshka, 3:4)

#no sibling relationship
plot(dat$age3_ln, dat$age4_ln)

#ricker
plot(dat$S, dat$lnRS)
rick <- lm(lnRS ~ S, data = dat)
summary(rick)
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))
#remove influential point
rick2 <- lm(lnRS ~ S, data = dat[-26,])
summary(rick2)

forecast::tsdisplay(residuals(rick))
forecast::auto.arima(rick$model$lnRS, xreg = rick$model$S)
rick_ts <- arima(rick$model$lnRS, order=c(1,0,0), xreg = rick$model$S, method = "ML")
AIC(rick, rick_ts)
rick_ts
dat$rickts_pred <- exp(pred_arima(rick_ts, dat$lnRS, dat$S)[1, ]) * dat$S

#Moving average
#You can do this model 2 ways, both are moving averages on the log scale
#The difference is if you use the mean (mu) or the median (md) when you go back the natural scale.
dat$mu5_pred <- pred_ma(dat$age4_ln, yrs = 5)[, "mean"]
dat$md5_pred <- pred_ma(dat$age4_ln, yrs = 5)[, "median"]

#Univariate
forecast::tsdisplay(dat$age4_ln)
forecast::auto.arima(dat$age4_ln)
mu_ts <- arima(dat$age4_ln, order=c(1,0,0))
temp <- pred_arima(mu_ts, x = dat$age4_ln)
#Here again you can use the mean (mu) or the median (md) when you go back the natural scale.
dat$mu_pred <- exp(temp[1,] + temp[2,]^2/2)
dat$md_pred <- exp(temp[1,])

#Exponential smoothing
ets <- forecast::ets(dat$age4_ln, "ANN")
dat$es_pred <- pred_es(dat$age4_ln)

comp_models(dat, 4)
#all of these models have struggled recently.
#Using medians is preferred across the board.
#The ricker model is heavily dependent on the arima component and has an outlier
#AR prefers the median from the univariate model
exp(predict(mu_ts, n.ahead = 1)$pred)

#The ricker model prediction is similar
tail(deshka)
exp(predict(rick_ts, 1, newxreg = 25490)[[1]]) * 25490

#I get a different ets prediction
exp(predict(ets, h = 1)[["mean"]][1])

#moving average
tail(dat)
exp(mean(dat$age4_ln[32:36]))
