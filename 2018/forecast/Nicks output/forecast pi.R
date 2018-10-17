library(magrittr)

#get data
load(".\\2018\\Nicks output\\age4.rda")
load(".\\2018\\Nicks output\\age5.rda")
load(".\\2018\\Nicks output\\age6.rda")

#caluclate return year and prep data
dat4$ryr <- dat4$byr + 4
dat5$ryr <- dat5$byr + 5
dat6$ryr <- dat6$byr + 6
dat4 <- dat4[dat4$ryr %in% 2013:2017, c("ryr", "age4", "ricker_pred")]
dat5 <- dat5[dat5$ryr %in% 2013:2017, c("ryr", "age5", "sib_pred")]
dat6 <- dat6[dat6$ryr %in% 2013:2017, c("ryr", "age6", "ma5_pred")]

#calculate errors
errors <- 
  dplyr::left_join(dat4, dat5, "ryr") %>%
  dplyr::left_join(dat6, "ryr") %>%
  dplyr::mutate(return = age4 + age5 + age6,
                hind = ricker_pred + sib_pred + ma5_pred,
                error = hind - return)

#rmse 80% bounds
sqrt(mean(errors$error^2)) * c(qt(.1, 5), qt(.9, 5))

