library(aslpack)
weir <- readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\2017 Deshka coho sex comp_DL.xlsx",
                         sheet = 3,
                         range = "A5:F56",
                         col_names = c("date", "passage", "samples", "strata"),
                         col_types = c("date", "numeric", rep("skip", 2), "numeric", "text"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
#No samples in strata 1, 2 or after strata 6
table(weir$samples == 0, weir$strata, useNA = "ifany")
#extend first and last strata to encoumpass all weir counts
weir$strata <- ifelse(weir$date < as.POSIXct("2017-07-30 UTC"), 3, weir$strata)
weir$strata <- ifelse(weir$date > as.POSIXct("2017-08-27 UTC"), 6, weir$strata)
table(weir$samples == 0, weir$strata, useNA = "ifany")

weir_strata <-
  weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage),
                   samples = sum(samples))
weir_sum <- dplyr::summarise(weir_strata, total = sum (total))

dat_sl <- readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\2017 Deshka coho sex comp_DL.xlsx",
                          sheet = 2,
                          range = "A3:F341",
                          col_names = c("date", "sex", "length", "strata"),
                          col_types = c("date", rep("skip", 2), "text", "numeric", "text"))
head(dat_sl)
tail(dat_sl)
hist(dat_sl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(dat_sl$length)
dat_sl$length[dat_sl$length ==55] <- 550
lapply(dat_sl, table, useNA = "ifany")


#Not expecting sampling in proportion to passage
plot(ecdf(rep(weir$date, times = weir$passage)))
plot(ecdf(rep(weir$date, times = weir$samples)), add = TRUE, col = "red")
ks.test(as.numeric(rep(weir$date, times = weir$passage)),
        as.numeric(rep(weir$date, times = weir$samples)))

#sex comp time invariant
tab <- table(dat_sl$strata,dat_sl$sex)
addmargins(tab)
chisq.test(tab)

pooled_sl <- asl(dat_sl, weir_sum) %>%
  asltable(totalname = "Weir Passage", output = "sl")
pooled_sl

strat_sl <-
  asl(dat_sl, weir_strata, "strata") %>%
  combine_strata() %>%
  asltable(totalname = "Weir passage", output = "sl")
strat_sl

# WriteXLS::WriteXLS(c("sl", "al", "asl"),
#                    "H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\17 Deska Chin.xlsx",
#                    SheetNames = c("Sex Comp", "Age Comp", "AgeSex comp"),
#                    BoldHeaderRow = TRUE)

