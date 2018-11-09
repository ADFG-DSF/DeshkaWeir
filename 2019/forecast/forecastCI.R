#Historic age4-6 forecast and actual
dat <- 
  data.frame(
    forecast = c(26810, 33337, 40753, 43805, 41041, 60833, 48687, 49071, 37007, 20268, 20593, 30775, 21080, 21665, 26791, 19063, 20418, 24638, 17813, 10595),
    actual = c(33371, 42273, 33210, 32955, 46193, 66383, 44134, 38451, 23648, 9708, 12531, 21605, 21410, 15096, 18876, 16068, 22928, 21209, 11500, 5237)
  )

#forecasts bias high, worst at small run sizes
ggplot2::ggplot(dat, ggplot2::aes(x = actual, y = forecast)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE) +
  ggplot2::geom_abline(slope = 1)
ggplot2::ggplot(dat[-6, ], ggplot2::aes(x = actual, y = forecast)) + 
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = TRUE) +
  ggplot2::geom_abline(slope = 1)

dat$resid <- log(dat$forecast) -log(dat$actual)
#CI on log scale
#forecast includes age3 while erorrs fo not.
s <- sqrt(sum(dat$resid^2)/dim(dat)[1])
t <- qt(.90, df = dim(dat)[1])
exp(log(10855) + s * c(-t , t))