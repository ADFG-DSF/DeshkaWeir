library(aslpack)
coho_weir <- 
  readxl::read_xlsx(".\\2018\\2018 Deshka coho sex comp_DL.xlsx",
                    sheet = 3,
                    range = "A5:F48",
                    col_names = c("date", "passage", "samples", "strata"),
                    col_types = c("date", "numeric", rep("skip", 2), "numeric", "text"))
head(coho_weir)
tail(coho_weir)
lapply(coho_weir, table, useNA = "ifany")
#No samples in strata 1 or after strata 6
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
#extend second and last strata to encoumpass all earlier (and later) weir counts
coho_weir$strata <- ifelse(coho_weir$date < as.POSIXct("2018-07-23 UTC"), 2, coho_weir$strata)
coho_weir$strata <- ifelse(coho_weir$date > as.POSIXct("2018-08-26 UTC"), 6, coho_weir$strata)
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
range(table(coho_weir$date))

coho_weir_strata <-
  coho_weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage),
                   samples = sum(samples))
coho_weir_sum <- dplyr::summarise(coho_weir_strata, total = sum (total))

coho_sl <- 
  readxl::read_xlsx(".\\2018\\2018 Deshka coho sex comp_DL.xlsx",
                    sheet = 2,
                    range = "A3:F302",
                    col_names = c("date", "sex", "length", "strata"),
                    col_types = c("date", rep("skip", 2), "text", "numeric", "text"))
head(coho_sl)
tail(coho_sl)
hist(coho_sl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(coho_sl$length)
coho_sl$length[coho_sl$length > 675]
coho_sl$length[coho_sl$length == 5758] <- 575
coho_sl$length[coho_sl$length == 4954] <- 495
hist(coho_sl$length)
lapply(coho_sl, table, useNA = "ifany")
coho_sl$sex <- toupper(coho_sl$sex)
lapply(coho_sl, table, useNA = "ifany")

#Not expecting sampling in proportion to passage
plot_ks(coho_weir, "passage")

#sex comp time invariant
tab_lr(coho_sl, "sex")

pooled_sl <- asl(coho_sl, coho_weir_sum) %>%
  tab_asl(totalname = "Weir Passage", output = "sl")
pooled_sl

strat_sl <-
  asl(coho_sl, coho_weir_strata, "strata") %>%
  combine_strata() %>%
  tab_asl(totalname = "Weir passage", output = "sl")
strat_sl

#Use pooled. point estiminates similar but se's smaller when pooled.

