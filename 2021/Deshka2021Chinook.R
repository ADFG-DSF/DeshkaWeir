library(magrittr)
source("H:\\My Documents\\R_functions\\ASL_funs.R")
weir <- readxl::read_xlsx(".\\2021\\Deshka Chinook Age Analysis 2021_DL.xlsx",
                          sheet = 2,
                          range = "A11:M94",
                          col_names = c("date", "passage", "samples", "strata"),
                          col_types = c("date", "numeric", rep("skip", 3), "guess", rep("skip", 6), "numeric"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
range(table(weir$date))
plot(weir$passage, weir$samples)
weir_sum <- data.frame(total = sum(weir$passage))

asl_dat <- readxl::read_xlsx(".\\2021\\Deshka Chinook Age Analysis 2021_DL.xlsx",
                             sheet = 4,
                             range = "D2:O371",
                             col_names = c("date", "length", "sex", "age", "strata"),
                             col_types = c("date", rep("skip", 2), rep("guess", 2), rep("skip", 5), "text", "numeric"))
head(asl_dat)
tail(asl_dat)
hist(asl_dat$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(asl_dat$length)
lapply(asl_dat, table, useNA = "ifany")
asl_dat$age[grepl("1.1", asl_dat$age)] <- 1.1
table(asl_dat$age, useNA = "ifany")

#Make sure strata definitions match
compare_strata(asl_dat, weir, passage) #Looks pretty good

#Not sampled in proportion to passage
plot_ks(weir, "passage")

#Second and third strata could be wider. Currently cross big migrations.
plot(weir$date, weir$passage, type = "l")
points(weir$date, weir$samples * max(weir$passage) / max(weir$samples), col = "red")
abline(v = as.POSIXct("2021-06-12 UTC"))
abline(v = as.POSIXct("2021-06-16 UTC"))
abline(v = as.POSIXct("2021-06-21 UTC"))

plot(weir$date, weir$passage, type = "l")
points(weir$date, weir$samples * max(weir$passage) / max(weir$samples), col = "red")
abline(v = as.POSIXct("2021-06-9 UTC"))
abline(v = as.POSIXct("2021-06-16 UTC"))
abline(v = as.POSIXct("2021-06-22 UTC"))
asl_dat$strata <- ifelse(asl_dat$strata == 1 & asl_dat$date >= as.POSIXct("2021-06-9 UTC"), 2, asl_dat$strata)
asl_dat$strata <- ifelse(asl_dat$strata == 4 & asl_dat$date <= as.POSIXct("2021-06-22 UTC"), 3, asl_dat$strata)
weir$strata <- ifelse(weir$strata == 1 & weir$date >= as.POSIXct("2021-06-9 UTC"), 2, weir$strata)
weir$strata <- ifelse(weir$strata == 4 & weir$date <= as.POSIXct("2021-06-22 UTC"), 3, weir$strata)
compare_strata(asl_dat, weir, passage) #Looks pretty good


#sex comp varies by strata
t.sex<-table(asl_dat$strata,asl_dat$sex)
addmargins(t.sex, c(1, 2))
chisq.test(t.sex)
DescTools::GTest(t.sex)

#age comp varies by strata
clean_age <- asl_dat[asl_dat$age %in% c("1.1", "1.2", "1.3", "1.4"), ]
t.age<-table(clean_age$strata,clean_age$age)
addmargins(t.age, c(1, 2))
chisq.test(t.age)
DescTools::GTest(t.age)

#Note 3 ways to proceed
#1) stratify sex comp and age comp independently (This is the past practice)
#2) produce a single stratified ASL table
#3) Stratify sex first and then produce and ASL table with the stratifed run by sex estimates

weir_strata <- 
  weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage))

#First option
sl <- 
  asl(asl_dat, weir_strata, groupvars = "strata") %>% 
  combine_strata() %>% 
  tab_asl(totalname = "Weir Passage", output = "sl", display_cols = "stat")
sl

al <- 
  asl(clean_age, weir_strata, groupvars = "strata") %>% 
  combine_strata() %>% 
  tab_asl(totalname = "Weir", output = "al", display_cols = "stat")
al

#Second option
chin_asl <- 
  asl(clean_age, weir_strata, groupvars = "strata") %>% 
  combine_strata() %>% 
  tab_asl(totalname = "Weir", output = "asl")
chin_asl

#Note: sex comps similar for aged and unaged fish
t.sexaged <- table(asl_dat$age %in% c("1.1", "1.2", "1.3", "1.4"), asl_dat$sex)
addmargins(t.sexaged, c(1, 2))
DescTools::GTest(t.sexaged)

#Third option
sl_time <- asl(asl_dat, weir_strata, groupvars = "strata") 
sl_tab <- 
  sl_time %>% 
  combine_strata() %>% 
  tab_asl(totalname = "Weir Passage", display_cols = "stat", output = "sl")
sl_tab

weir_sextime <-   
  sl_time[sl_time$age %in% "All", ] %>%
  dplyr::select(strata, sex_strata = sex, total = t.z, se_total = sd_t.z)
clean_age$sex_strata = clean_age$sex


asl_tab <- 
  asl(clean_age, weir_sextime, c("strata", "sex_strata")) %>% 
  combine_strata() %>% 
  tab_asl(totalname = "Weir", output = "asl", overall_se = 0)
asl_tab

#These options are functionally identical. I'm going to present the first option becaseu its what these guys are used to.

#output excel file with age and sex comps
WriteXLS::WriteXLS(c("sl", "al", "chin_asl"),
                   ".\\2021\\21 Deshka ASL.xlsx",
                   SheetNames = c("Chinook sex comp", "Chinook age comp", "Chinook agesex comp"),
                   AdjWidth = TRUE,
                   BoldHeaderRow = TRUE)

