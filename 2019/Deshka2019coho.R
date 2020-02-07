library(aslpack)
coho_weir <- 
  readxl::read_xlsx(".\\2019\\2019 Deshka coho sex comp_DL.xlsx",
                    sheet = 3,
                    range = "A5:F59",
                    col_names = c("date", "passage", "samples", "strata"),
                    col_types = c("date", "numeric", rep("skip", 2), "numeric", "text"))
head(coho_weir)
tail(coho_weir)
lapply(coho_weir, table, useNA = "ifany")
#No samples in strata 1, strata 6 too big
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
#extend second strata to encoumpass all earlier weir counts
coho_weir$strata <- ifelse(coho_weir$date < as.POSIXct("2019-07-23 UTC"), 2, coho_weir$strata)
#split last strat into two
coho_weir$strata <- ifelse(coho_weir$date > as.POSIXct("2019-08-27 UTC"), 7, coho_weir$strata)
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
range(table(coho_weir$date))

coho_weir_strata <-
  coho_weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage),
                   samples = sum(samples))
coho_weir_sum <- dplyr::summarise(coho_weir_strata, total = sum (total))

coho_sl <- 
  readxl::read_xlsx(".\\2019\\2019 Deshka coho sex comp_DL.xlsx",
                    sheet = 4,
                    range = "A5:E265",
                    col_names = c("date", "sex", "length"),
                    col_types = c("date", rep("skip", 2), "text", "numeric"))
head(coho_sl)
tail(coho_sl)
hist(coho_sl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(coho_sl$length)
lapply(coho_sl, table, useNA = "ifany")
lapply(as.character(2:7), function(x) range(coho_weir$date[coho_weir$strata == x]))
coho_sl$strata <- cut(coho_sl$date, 
               breaks = as.POSIXct(c("2019-7-16 UTC","2019-7-29 UTC", "2019-8-5 UTC", "2019-8-12 UTC", "2019-8-19 UTC", "2019-8-27 UTC", "2019-9-8 UTC")),
               labels = as.character(2:7),
               right = FALSE)
table(coho_sl$date, coho_sl$strata, useNA = "ifany") #matches weir strata

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

