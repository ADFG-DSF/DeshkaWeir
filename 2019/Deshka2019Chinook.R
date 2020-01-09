library(aslpack)
weir <- readxl::read_xlsx(".\\2019\\Copy of 2019 Deshka Chinook Age Analysis_DL_AR.xlsx",
                          sheet = 2,
                          range = "A11:M123",
                          col_names = c("date", "passage", "samples", "strata"),
                          col_types = c("date", "numeric", rep("skip", 3), "guess", rep("skip", 6), "numeric"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
range(table(weir$date))
plot(weir$passage, weir$samples)
weir_sum <- data.frame(total = sum(weir$passage))

asl_dat <- readxl::read_xlsx(".\\2019\\Copy of 2019 Deshka Chinook Age Analysis_DL_AR.xlsx",
                             sheet = 4,
                             range = "C2:K272",
                             col_names = c("date", "length", "sex", "age", "strata"),
                             col_types = c("date", rep("skip", 2), rep("guess", 2), rep("skip", 2), "text", "numeric"))
head(asl_dat)
tail(asl_dat)
hist(asl_dat$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(asl_dat$length)
lapply(asl_dat, table, useNA = "ifany")
asl_dat$age[grepl("1.1", asl_dat$age)] <- 1.1
table(asl_dat$age, useNA = "ifany")

#Make sure strata definitions match
compare_strata(asl_dat, weir, passage) #Looks pretty good

#NOT sampled in proportion to passage
plot_ks(weir, "passage")

#sex comp time invariant
t.sex<-table(asl_dat$strata,asl_dat$sex)
addmargins(t.sex, c(1, 2))
chisq.test(t.sex)
DescTools::GTest(t.sex)

sl <- asl(asl_dat, weir_sum) %>% tab_asl(totalname = "Weir Passage", output = "sl", display_cols = "stat")
sl

#No need to stratify for age after restricting to dominat ages
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

al0 <- asl(clean_age, weir_sum)
al<- al0 %>% tab_asl(totalname = "Weir", output = "al", display_cols = "stat")
al


asl <- asl(clean_age, weir_sum) %>% tab_asl(totalname = "Weir", output = "asl")
asl

#output excel file with age and sex comps
WriteXLS::WriteXLS(c("sl", "al", "asl"),
                   ".\\2019\\19 Deshka ASL.xlsx",
                   SheetNames = c("Chinook sex comp", "Chinook age Comp", "Chinook agesex comp"),
                   AdjWidth = TRUE,
                   BoldHeaderRow = TRUE)

#Update brood table in preseason package
brood <- preseason::deshka
deshka <- rbind(brood,
               data.frame(byr = 2019, S = NA, age3 = NA, age4 = NA, age5 = NA, age6 = NA))
tail(deshka, n = 10)
#10/29/19 email from nick updates NCI marine harvest for 2018 and 2019
#Only Tyonek subsitance and Northern district Harvest of Deshka Chinook, C&R inriver
Hm <- c(630, 697)
Hm_Deshka <- Hm * 0.136 #average of 2016 and 2017 (both fisheries) from SusitnaEG HM.xlxs

#update 2018
p.z_2018 <- c(.386, .250, .352, .011) #P.z and t.z from 2018 from html file
Hm_Deshka2018 <- Hm_Deshka[1] * c(.386, .250, .352, .011)
t.z_2018 <- c(3303, 2137, 3011, 97)
N_2018 <- c(3303, 2137, 3011, 97) + Hm_Deshka2018
#need harvest
deshka$age3[deshka$byr == 2015] <- as.integer(N_2018[1])
deshka$age4[deshka$byr == 2014] <- as.integer(N_2018[2])
deshka$age5[deshka$byr == 2013] <- as.integer(N_2018[3])
deshka$age6[deshka$byr == 2012] <- as.integer(N_2018[4])
tail(deshka, n = 10)

#Add 2019
Hm_Deshka2019 <- Hm_Deshka[2] * al0[(al0$sex == "Both" & al0$age != "All"), "p.z"]
N_2019 <- al0[(al0$sex == "Both" & al0$age != "All"), "t.z"] + Hm_Deshka2019
  
deshka$S[deshka$byr == 2019] <- new_dat$t.z[new_dat$age == "All"]
#need harvest
deshka$age3[deshka$byr == 2016] <- as.integer(N_2019[1,])
deshka$age4[deshka$byr == 2015] <- as.integer(N_2019[2,])
deshka$age5[deshka$byr == 2014] <- as.integer(N_2019[3,])
deshka$age6[deshka$byr == 2013] <- as.integer(N_2019[4,])
tail(deshka, n = 10)

#save(deshka, file="H:\\My Documents\\preseason\\data\\deshka.rda")

