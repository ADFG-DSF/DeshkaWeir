library(aslpack)
weir <- readxl::read_xlsx(".\\2018\\2018 Deshka Chinook Age Analysis_DL.xlsx",
                         sheet = 2,
                         range = "A11:M102",
                         col_names = c("date", "passage", "samples", "strata"),
                         col_types = c("date", "numeric", rep("skip", 3), "guess", rep("skip", 6), "numeric"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
range(table(weir$date))
plot(weir$passage, weir$samples)
weir_sum <- data.frame(total = sum(weir$passage))

asl_dat <- readxl::read_xlsx(".\\2018\\2018 Deshka Chinook Age Analysis_DL.xlsx",
                          sheet = 4,
                          range = "A5:J248",
                          col_names = c("date", "sex", "length", "age", "strata"),
                          col_types = c("date", rep("skip", 2), rep("guess", 2), rep("skip", 3), "text", "numeric"))
head(asl_dat)
tail(asl_dat)
hist(asl_dat$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(asl_dat$length)
lapply(asl_dat, table, useNA = "ifany")
asl_dat$age[grepl("1.1", asl_dat$age)] <- 1.1
table(asl_dat$age, useNA = "ifany")

#Make sure strata definitions match
compare_strata(asl_dat, weir, passage)
breaks <- c(as.POSIXct("2018-05-1 UTC"),
            as.POSIXct("2018-06-16 UTC"),
            as.POSIXct("2018-06-21 UTC"),
            as.POSIXct("2018-06-25 UTC"),
            as.POSIXct("2018-08-31 UTC"))
weir$strata <- cut(weir$date, breaks, labels = 1:4)
asl_dat$strata <- cut(asl_dat$date, breaks, labels = 1:4)
compare_strata(asl_dat, weir, passage)

#NOT sampled in proportion to passage
#sampled are not in proportion to passage
plot_ks(weir, "passage")

#sex comp time invariant
t.sex<-table(asl_dat$strata,asl_dat$sex)
addmargins(t.sex, c(1, 2))
chisq.test(t.sex)
DescTools::GTest(t.sex)

sl <- asl(asl_dat, weir_sum) %>% tab_asl(totalname = "Weir Passage", output = "sl", display_cols = "stat")
sl

#No need to stratify for age
clean_age <- asl_dat[asl_dat$age %in% c("1.1", "1.2", "1.3", "1.4"), ]
t.age<-table(clean_age$strata,clean_age$age)
addmargins(t.age, c(1, 2))
chisq.test(t.age)
DescTools::GTest(t.age)

red_age <- clean_age[clean_age$age %in% c("1.1", "1.2", "1.3"), ]
t.age2<-table(red_age$strata,red_age$age)
addmargins(t.age2, c(1, 2))
chisq.test(t.age2)
DescTools::GTest(t.age2)

al <- asl(clean_age, weir_sum) %>% tab_asl(totalname = "Weir", output = "al", display_cols = "stat")
al


asl <- asl(clean_age, weir_sum) %>% tab_asl(totalname = "Weir", output = "asl")
asl

#Use 2018 asl to update Deshka brood table in preseason package
brood <- preseason::deshka
deshka <- rbind(brood,
               data.frame(byr = 2018, S = NA, age3 = NA, age4 = NA, age5 = NA, age6 = NA))
tail(deshka, n = 10)
#Only Tyonek subsitance Harvest of Deshka Chinook, C&R inriver
Hm <- 500 * mean(c(244, 175) / c(3118, 2735))
new_dat <- 
  asl(clean_age, weir_sum) %>%
  dplyr::filter(sex == "Both") %>%
  dplyr::mutate(H = Hm * p.z,
                N = H + t.z)
  
deshka$S[deshka$byr == 2018] <- new_dat$t.z[new_dat$age == "All"]
#need harvest
deshka$age3[deshka$byr == 2015] <- as.integer(new_dat$N[new_dat$age == "1.1"])
deshka$age4[deshka$byr == 2014] <- as.integer(new_dat$N[new_dat$age == "1.2"])
deshka$age5[deshka$byr == 2013] <- as.integer(new_dat$N[new_dat$age == "1.3"])
deshka$age6[deshka$byr == 2012] <- as.integer(new_dat$N[new_dat$age == "1.4"])
tail(deshka, n = 10)

#devtools::use_data(deshka, pkg = "H:\\My Documents\\preseason", overwrite = TRUE)
