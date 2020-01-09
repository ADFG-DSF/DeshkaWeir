#Reads nick's brood table
brood <- read.csv("H:\\My Documents\\DeshkaWeir\\2020\\forecast\\Nick\\Deshka2020.csv")

#prep for analysis
library(preseason)
dat <- prep_brood(brood, 3:5)

#sibling model
table(dat$age3_ln) #some 0's for age 3
dat$age3_ln <- ifelse(is.infinite(dat$age3_ln), log(1), dat$age3_ln)
table(dat$age3_ln) #clunky but OK
#Sibling relationship w 4's but relationship w 3's based heavily on 0 counts
ggplot2::ggplot(dat, ggplot2::aes(x = age4_ln, y = age5_ln, size = age3_ln)) + ggplot2::geom_point()
sib <- lm(age5_ln ~ age4_ln + age3_ln, data = dat)
summary(sib)
par(mfrow = c(2,2)); plot(sib); par(mfrow = c(1,1))
forecast::tsdisplay(residuals(sib))
temp <- pred_lm(sib)
dat$sib_pred <- exp(temp[1,] + temp[2,]^2/2)
dat$sibmd_pred <- exp(temp[1,])

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
dat$mu5_pred <- pred_ma(dat$age5_ln, yrs = 5)[, "mean"]
dat$md5_pred <- pred_ma(dat$age5_ln, yrs = 5)[, "median"]

#Univariate
forecast::tsdisplay(dat$age5_ln)
forecast::auto.arima(dat$age5_ln)
mu_ts1 <- arima(dat$age5_ln, order=c(0,0,2))
mu_ts2 <- arima(dat$age5_ln, order=c(1,0,0))
AIC(mu_ts1, mu_ts2)
temp <- pred_arima(mu_ts2, x = dat$age5_ln)
dat$muarima_pred <- exp(temp[1,] + temp[2,]^2/2)
dat$mdarima_pred <- exp(temp[1,])

#Exponential smoothing
forecast::ets(dat$age5_ln, "ANN")
dat$es_pred <- pred_es(dat$age5_ln)

comp_models(dat, 5)
#again note using the median when going back to the natural scale is prefered across all models
#AR prefers 5 year mean with the median backtransform
#moving average
tail(dat[, 1:9])
exp(mean(dat$age5_ln[37:41]))

#Sibling
tail(deshka, 7)
pred_20 <- predict(sib, newdata = data.frame(age4_ln = log(1370), age3_ln = log(3336)), se.fit = TRUE)
exp(pred_20$fit)


#exponential smoothing
ets <- forecast::ets(dat$age5_ln, "ANN")
exp(predict(ets, h = 1)[["mean"]][1])

#time series
exp(predict(mu_ts2, n.ahead = 1)$pred)

#ricker 
tail(deshka, 10)
exp(predict(rick_ar1, 1, newxreg = 25490)[[1]]) * 25490
