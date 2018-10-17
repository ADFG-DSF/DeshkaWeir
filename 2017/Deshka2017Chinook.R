library(aslpack)
weir <- readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\2017 Deshka Chinook Age Analysis_DL.xlsx",
                         sheet = 2,
                         range = "A11:M121",
                         col_names = c("date", "passage", "samples", "strata"),
                         col_types = c("date", "numeric", rep("skip", 3), "guess", rep("skip", 6), "numeric"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
weir_sum <-
  weir %>%
  dplyr::select(passage) %>%
  dplyr::summarise(total = sum(passage))

asl <- readxl::read_xlsx("H:\\My Documents\\Deshka R, Chinook and Coho\\2017\\2017 Deshka Chinook Age Analysis_DL.xlsx",
                          sheet = 4,
                          range = "A5:J287",
                          col_names = c("date", "sex", "length", "age", "strata"),
                          col_types = c("date", rep("skip", 2), rep("guess", 2), rep("skip", 3), "text", "numeric"))
head(asl)
tail(asl)
hist(asl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(asl$length)
lapply(asl, table, useNA = "ifany")
asl$age[grepl("1.1", asl$age)] <- 1.1
table(asl$age, useNA = "ifany")

#sampled in proportion to passage
plot(ecdf(rep(weir$date, times = weir$passage)), main = "Passage and samples ECD in time", ylab = "Proportion", xaxt = "n")
axis(1, weir$date, format(weir$date, "%b %d"))
plot(ecdf(rep(weir$date, times = weir$samples)), add = TRUE, col = "red")
legend("right", legend = c("weir passage", "asl samples"), col = c("black", "red"), pch = 16)
ks.test(as.numeric(rep(weir$date, times = weir$passage)),
        as.numeric(rep(weir$date, times = weir$samples)))

#sex comp time invariant
t.sex<-table(asl$strata,asl$sex)
addmargins(t.sex, c(1, 2))
chisq.test(t.sex)
DescTools::GTest(t.sex)

sl <- asl(asl, weir_sum) %>% asltable(totalname = "Weir Passage", display_cols = "stat", output = "sl")
sl

#No need to stratify for age
clean_age <- asl[asl$age %in% c("1.1", "1.2", "1.3", "1.4"), ]
t.age<-table(clean_age$strata,clean_age$age)
addmargins(t.age, c(1, 2))
chisq.test(t.age)
DescTools::GTest(t.age)

red_age <- clean_age[clean_age$age %in% c("1.2", "1.3"), ]
t.age2<-table(red_age$strata,red_age$age)
addmargins(t.age2, c(1, 2))
chisq.test(t.age2)
DescTools::GTest(t.age2)

al <- asl(clean_age, weir_sum) %>% asltable(totalname = "Weir", display_cols = "stat", output = "al")
al


asl <- asl(clean_age, weir_sum) %>% asltable(totalname = "Weir", output = "asl")
asl

