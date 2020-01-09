#Reads nick's brood table
brood <- read.csv("H:\\My Documents\\DeshkaWeir\\2020\\forecast\\Nick\\Deshka2020.csv")

#prep for analysis
library(preseason)
dat <- prep_brood(brood, 4:6)

#sibling model
#Sibling relationship w 5's but I dont see a relationship w 4's
ggplot2::ggplot(dat, ggplot2::aes(x = age5_ln, y = age6_ln, size = age4_ln)) + ggplot2::geom_point()
sib <- lm(age6_ln ~ age5_ln + age4_ln, data = dat)
summary(sib)
sib <- update(sib, formula. = . ~ age5_ln) #drop 4's
summary(sib)
par(mfrow = c(2,2)); plot(sib); par(mfrow = c(1,1))
ggplot2::ggplot(dat, ggplot2::aes(x = age5_ln, y = age6_ln)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method=lm, se=TRUE)
forecast::tsdisplay(residuals(sib))
forecast::auto.arima(dat$age6_ln, xreg = dat$age5_ln)
sib_arima <- arima(dat$age6_ln, c(1,0,0), xreg = dat$age5_ln)
summary(sib_arima)

temp <- pred_arima(sib_arima, x = dat$age6_ln, xreg = dat$age5_ln)
dat$sib_pred <- exp(temp[1,] + temp[2,]^2/2)
dat$sibmd_pred <- exp(temp[1,])

#ricker
plot(dat$S, dat$lnRS)
rick <- lm(lnRS ~ S, data = dat) #no relationship!
summary(rick) #model insignificant
par(mfrow = c(2,2)); plot(rick); par(mfrow = c(1,1))

#Moving average
dat$mu5_pred <- pred_ma(dat$age6_ln, yrs = 5)[, "mean"]
dat$md5_pred <- pred_ma(dat$age6_ln, yrs = 5)[, "median"]

#univariate
forecast::tsdisplay(dat$age6_ln)
forecast::auto.arima(dat$age6_ln) # wants to difference
mu_ar1 <- arima(dat$age6_ln, order=c(1,1,0))
summary(mu_ar1)
temp <- pred_arima(mu_ar1, x = dat$age6_ln)
dat$mu_pred <- exp(temp[1,] + temp[2,]^2/2)
dat$md_pred <- exp(temp[1,])

#exponential smooting
ets <- forecast::ets(dat$age6_ln)
dat$es_pred <- pred_es(dat$age6_ln)

comp_models(dat, 6)
#ets
exp(predict(ets, h = 1)[["mean"]][1])

#the moving average model w median transform
tail(dat)
exp(mean(dat$age6_ln[36:40]))
#note alot of skew if you transform to the mean
exp(mean(dat$age6_ln[36:40]) + var(dat$age6_ln[36:40])/2)

#sibling
tail(deshka, 10)
exp(predict(sib_arima, n.ahead = 1, newxreg = log(7225))$pred)

#time series
exp(predict(mu_ar1, n.ahead = 1)$pred)


