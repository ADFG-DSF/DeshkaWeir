library(preseason)
dat <- prep_brood(deshka, 3)

#Moving average
dat$md5_pred <- pred_ma(dat$ln, yrs = 5)[, "median"]

#Univariate
forecast::tsdisplay(dat$ln)
forecast::auto.arima(dat$ln)
mu_ts <- arima(dat$ln, order=c(0,1,0))
temp <- pred_arima(mu_ts, x = dat$ln)
dat$ts_pred <- exp(temp[1,])

#Exponential smoothing
ets <- forecast::ets(dat$ln, "ANN")
dat$es_pred <- pred_es(dat$ln)

comp_models(dat, 3)
#AR thinks we should use the 5 year moving average dispite the MAD. THe data is just too sparse to thrust the other methods.
tail(dat)
exp(mean(dat$ln[33:37]))

exp(forecast::forecast(ets, 1)[["mean"]])
