library(magrittr)
source("H:\\My Documents\\R_functions\\ASL_funs.R")
weir <- readxl::read_xlsx(".\\2020\\2020 Deshka Chinook Age Analysis_DL.xlsx",
                          sheet = 2,
                          range = "A11:M82",
                          col_names = c("date", "passage", "samples", "strata"),
                          col_types = c("date", "numeric", rep("skip", 3), "guess", rep("skip", 6), "numeric"))
head(weir)
tail(weir)
lapply(weir, table, useNA = "ifany")
range(table(weir$date))
plot(weir$passage, weir$samples)
weir_sum <- data.frame(total = sum(weir$passage))

asl_dat <- readxl::read_xlsx(".\\2020\\2020 Deshka Chinook Age Analysis_DL.xlsx",
                             sheet = 4,
                             range = "D2:L346",
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

#Sampled in proportion to passage
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



al0 <- asl(clean_age, weir_sum)
al<- al0 %>% tab_asl(totalname = "Weir", output = "al", display_cols = "stat")
al


asl <- asl(clean_age, weir_sum) %>% tab_asl(totalname = "Weir", output = "asl")
asl

#output excel file with age and sex comps
WriteXLS::WriteXLS(c("sl", "al", "asl"),
                   ".\\2020\\20 Deshka ASL.xlsx",
                   SheetNames = c("Chinook sex comp", "Chinook age Comp", "Chinook agesex comp"),
                   AdjWidth = TRUE,
                   BoldHeaderRow = TRUE)