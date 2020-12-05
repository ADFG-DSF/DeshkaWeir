source("H:\\My Documents\\R_functions\\ASL_funs.R")
library(magrittr)
coho_weir <- readxl::read_xlsx(".\\2016\\2016 Deshka Coho Sex Comp_DL_AR.xlsx",
                         sheet = 3,
                         range = "A5:G62",
                         col_names = c("date", "count", "samples", "strata"),
                         col_types = c("date", "numeric", rep("skip", 3), "numeric", "text"))
head(coho_weir)
tail(coho_weir)
lapply(coho_weir, table, useNA = "ifany")
range(table(coho_weir$date))
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")

#Weir flooded on these dates
plot(coho_weir$date, coho_weir$count) #Weir partially flooded Aug 9-12 and 25-28
rect(as.POSIXct("2016-08-8 UTC"), 0, as.POSIXct("2016-08-12 UTC"), 1500 ,col = rgb(0.5,0.5,0.5,1/4))
rect(as.POSIXct("2016-08-24 UTC"), 0, as.POSIXct("2016-08-28 UTC"), 1500 ,col = rgb(0.5,0.5,0.5,1/4))
#Change partial counts on flood dates to NA
coho_weir$count_full <- coho_weir$count
coho_weir$count_full[(coho_weir$date > "2016-08-8 UTC" & coho_weir$date <= "2016-08-12 UTC") | 
                     (coho_weir$date > "2016-08-24 UTC" & coho_weir$date <= "2016-08-28 UTC")] <- NA
plot(coho_weir$date, coho_weir$count_full) #Weir partially flooded Aug 9-12 and 25-28
rect(as.POSIXct("2016-08-8 UTC"), 0, as.POSIXct("2016-08-12 UTC"), 1500 ,col = rgb(0.5,0.5,0.5,1/4))
rect(as.POSIXct("2016-08-24 UTC"), 0, as.POSIXct("2016-08-28 UTC"), 1500 ,col = rgb(0.5,0.5,0.5,1/4))
#add regression independent var
coho_weir$jday <- as.integer(format(coho_weir$date, "%j")) - min(as.integer(format(coho_weir$date, "%j"))) + 1
# Identify flood days
flood <- coho_weir$jday[is.na(coho_weir$count_full)]

# #Try a smooth spline
# mod <- smooth.spline(coho_weir$jday[!is.na(coho_weir$count_full)], coho_weir$count_full[!is.na(coho_weir$count_full)], )
# preds <- predict(mod, data.frame(jday = 1:58))
# plot(coho_weir$jday, coho_weir$count_full, xlim = c(1, 58))
# lines(1:58, preds$y[[1]], col = "green")
# #? too wiggly...just interpolates
# 
# #Rought plot of cauchy, normal and logistic densities as run timing patterns (using prior years means)
# #Cauchy has the sharpest peak and the fattest tails... looks most like this data
# 
# #80% of run passed in 9 days
# coho_weir$rt <- cumsum(coho_weir$count) / sum(coho_weir$count)
# coho_weir[, c("date", "rt")] %>% print(n = 100)
# qnorm(.1, 0, 1); qnorm(.9, 0, 1);
# qlogis(.1, 0, 1); qlogis(.9, 0, 1);
# qcauchy(.1, 0, 1); qcauchy(.9, 0, 1);
# 
# plot(coho_weir$jday, coho_weir$count_full, xlim = c(1, 58))
# lines(1:58, 7000 * dcauchy(1:58, location = 30, scale = 9/6.15))
# lines(1:58, sum(coho_weir$count) * dnorm(1:58, mean = 30, sd = 9/2.56), col = "red")
# lines(1:58, 7000 * dlogis(1:58, location = 30, scale = 9/4.39), col = "blue")
# 
# #Model daily weir count as neg-binomial with cauchy run timing.
# #Total poorly estimated... runs up against prior suport even for v large upper bounds
# breaks <- c(0, which(diff(flood) != 1), length(flood))
# fday <- lapply(seq(length(breaks) - 1), function(i) range(flood[(breaks[i] + 1):breaks[i+1]]))
# 
# dat <- list(N = dim(coho_weir)[1] - length(flood),
#             w = coho_weir$count_full[!(coho_weir$jday %in% flood)],
#             day = coho_weir$jday[!(coho_weir$jday %in% flood)],
#             flood1 = fday[[1]],
#             flood2 = fday[[2]]
# )
# 
# library(rstan)
# fit <- stan(".\\2016\\fill_stan.stan",
#             pars = c("rt_mu", "rt_80", "weir", "phi", "p_f1", "weir_f1", "p_f2", "weir_f2"),
#             data = dat)
# 
# shinystan::launch_shinystan(fit)
# #pairs(fit)
# print(fit, digits_summary = 3)
# #Weir passage and passage estimates
# plot(coho_weir$jday, coho_weir$count_full)
# est <- summary(fit)$summary
# #points(jday, est[grepl("w_mu", names(est))], col = "red")
# lines(1:dim(coho_weir)[1],
#       est[grepl("weir$", rownames(est)), "mean"] *
#         # (plogis((1:125), est[grepl("rt_mu", rownames(est)), "mean"], est[grepl("rt_80", rownames(est)), "mean"]/4.39) -
#         #    plogis((0:124), est[grepl("rt_mu", rownames(est)), "mean"], est[grepl("rt_80", rownames(est)), "mean"]/4.39))
#         (pcauchy((1:dim(coho_weir)[1]), est[grepl("rt_mu", rownames(est)), "mean"], est[grepl("rt_80", rownames(est)), "mean"]/6.15) -
#            pcauchy((1:dim(coho_weir)[1] - 1), est[grepl("rt_mu", rownames(est)), "mean"], est[grepl("rt_80", rownames(est)), "mean"]/6.15))
# )


# #Instead gam instead
# #Note tried spline but prefer gam so I can specify the distribution
# library(mgcv)
# mod1 <- gam(count_full ~ s(jday), data = coho_weir, family = "poisson")
# par(mfrow = c(2, 2)); gam.check(mod1); par(mfrow = c(1, 1));
# summary(mod1)
# AIC(mod1)
# 
# #negative binomial looks a lot better
# mod2 <- gam(count_full ~ s(jday), data = coho_weir, family = "nb")
# par(mfrow = c(2, 2)); gam.check(mod2); par(mfrow = c(1, 1));
# summary(mod2)
# AIC(mod2)
# 
# #but the predictions lean towards too smooth
# pred <- predict(mod2, newdata = data.frame(jday = 1:58), type = "response", se = T)
# plot(coho_weir$jday, coho_weir$count_full)
# points(coho_weir$jday[coho_weir$jday %in% flood], coho_weir$count[coho_weir$jday %in% flood], col = "red")
# lines(1:58, pred$fit)
# pred$fit[flood]
# sum(pred$fit[flood])

#try splines
library(splines)
#AIC model selection picks a pretty squigly line
pass <- coho_weir$count_full[!is.na(coho_weir$count_full)]
  pass <- ifelse(pass == 0, 1, pass) # allow log
jd <- coho_weir$jday[!is.na(coho_weir$count_full)]

aicval <- numeric(25)
for(i in 5:30){aicval[i - 4] <- AIC(lm(log(pass) ~ bs(jd, i)))}
cbind(5:30, aicval)
plot(5:30, aicval, type = "l")
plot(coho_weir$jday, coho_weir$count_full)
hold <- lm(log(pass) ~ bs(jd, 16))
lines(1:58, exp(predict(hold, newdata = data.frame(jd = 1:58))))
exp(predict(hold, newdata = data.frame(jd = flood)))
sum(exp(predict(hold, newdata = data.frame(jd = flood))))

#Try PRESS
delresid <- function(lm.obj){
  out <- resid(lm.obj)/(1 - influence(lm.obj)$hat)
  return(out)
}
pressval <- numeric(25)
for(i in 5:30){
  del = delresid(lm(log(pass) ~ bs(jd, i)))
  pressval[i-4] <- sum(del^2)
}
#Yay, same result as AIC!
cbind(5:30, round(pressval, 1))

par(mfrow = c(2, 2))
plot(hold)
par(mfrow = c(1, 1))
plot(coho_weir$jday, exp(predict(hold, newdata = data.frame(jd = 1:58), interval = "confidence")[,3]), type = "n")
polygon(c(rev(1:58), 1:58), 
        c(rev(exp(predict(hold, newdata = data.frame(jd = 1:58), interval = "confidence")[,2])),
          exp(predict(hold, newdata = data.frame(jd = 1:58), interval = "confidence")[,3])),
        border = NA, col = "grey80")
points(coho_weir$jday, coho_weir$count_full)
lines(1:58, exp(predict(hold, newdata = data.frame(jd = 1:58), interval = "confidence")[,1]))

library(ggplot2)
dat_plot0 <-
  predict(hold, newdata = data.frame(jd = 1:58),se.fit = TRUE ,interval = "confidence") %>%
  as.data.frame() %>%
  dplyr::mutate(fit = fit.fit + se.fit^2/2) %>%
  dplyr::select(fit, lwr = fit.lwr, upr = fit.upr) %>%
  exp() %>%
  cbind(coho_weir[, c("date", "jday", "count")]) %>%
  as.data.frame() %>%
  dplyr::mutate(type = ifelse(jday %in% flood, "Estimated", "Full Count"),
                y = ifelse(jday %in% flood, fit, count),
                date = as.Date(date))
dat_plot <- 
  dat_plot0[dat_plot0$type == "Estimated", ] %>%
  dplyr::mutate(type = "Partial Count",
                y = count) %>%
  rbind(dat_plot0)
ggplot(dat_plot, aes(x = date, y = y, shape = type)) +
    geom_point(size = 3) +
    geom_line(aes(x = date, y = fit), inherit.aes = FALSE) +
    geom_segment(aes(x = date, xend = date, y = count, yend = y), dat_plot[dat_plot$type == "Estimated", ], inherit.aes = FALSE) +
    geom_ribbon(aes(x = date, ymin = lwr, ymax = upr), alpha = 0.1, inherit.aes = FALSE) +
    theme_bw(base_size = 20) +
    xlab("Date") +
    ylab("Count") +
    scale_shape_manual(name = "Count Type", values = c("Full Count" = 19, "Estimated" = 17, "Partial Count" = 1)) +
    scale_x_date(date_breaks = "weeks", date_labels = "%b-%d") +
    ggtitle("Coho passage, Little Susitna Weir, 2016")
    


#extend first to encoumpass all weir counts
#combine first and second strata since only 13 fish sampled in first strata
coho_weir$strata <- ifelse(coho_weir$date <= as.POSIXct("2016-07-22 UTC"), 2, coho_weir$strata)
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
#extend last to encoumpass all weir counts
coho_weir$strata <- ifelse(coho_weir$date > as.POSIXct("2016-08-27 UTC"), 6, coho_weir$strata)
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
lapply(as.character(2:6), function(x) range(coho_weir$date[coho_weir$strata == x]))


fit <- predict(hold, newdata = data.frame(jd = flood), se.fit = TRUE)

coho_weir$passage <- coho_weir$count
coho_weir$passage[coho_weir$jday %in% flood] <- exp(fit$fit + fit$residual.scale/2)
coho_weir$se_passage <- 0
coho_weir$se_passage[coho_weir$jday %in% flood] <- sqrt(exp(fit$fit)^2*fit$se.fit^2)
coho_weir %>% print(n = 100) 

coho_weirstrata <-
  coho_weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage),
                   se_total = sqrt(sum(se_passage^2)),
                   samples = sum(samples))
coho_weirsum <- coho_weirstrata %>% dplyr::summarise(total = sum(total), se_total = sqrt(sum(se_total^2)), samples = sum(samples))

coho_sl <- readxl::read_xlsx(".\\2016\\2016 Deshka Coho Sex Comp_DL_AR.xlsx",
                          sheet = 2,
                          range = "A3:F307",
                          col_names = c("date", "sex", "length", "strata"),
                          col_types = c("date", rep("skip", 2), "text", "numeric", "text"))
head(coho_sl)
tail(coho_sl)
hist(coho_sl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(coho_sl$length)
lapply(coho_sl, table, useNA = "ifany")
lapply(as.character(1:6), function(x) range(coho_sl$date[coho_sl$strata == x])) # matches weir


#Make sure strata definitions match
compare_strata(coho_sl, coho_weir, passage)

#Not expecting sampling in proportion to passage
plot_ks(coho_weir, "passage")

#sex comp time invariant
lr_sex18 <- tab_lr(coho_sl, "sex")
lr_sex18

coho_sltab <- asl(coho_sl, coho_weirsum) %>% tab_asl(totalname = "Weir Passage", 
                                                     output = "sl", 
                                                     gather_cols = c("n.z", "p.z", "ci_p.z", "lg.z", "range_lg.z"))
coho_sltab

coho_sltabstrat <-
  asl(coho_sl, coho_weirstrata, "strata") %>%
  combine_strata() %>%
  tab_asl(totalname = "Weir passage", 
          output = "sl", 
          gather_cols = c("n.z", "p.z", "ci_p.z", "lg.z", "range_lg.z"))
coho_sltabstrat
#estimates are similar... used pooled.

#95% CI
#hokey method
#subtract mean for excell error bar format
exp(log(9535) + c(-1.95, 1.95) * sqrt((485/9535)^2)) - 9535
