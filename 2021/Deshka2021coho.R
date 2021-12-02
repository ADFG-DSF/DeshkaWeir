library(magrittr)
source("H:\\My Documents\\R_functions\\ASL_funs.R")
coho_weir <- 
  readxl::read_xlsx(".\\2021\\2021 Deshka coho sex comp_DL.xlsx",
                    sheet = 3,
                    range = "A5:F35",
                    col_names = c("date", "passage", "samples", "strata"),
                    col_types = c("date", "numeric", rep("skip", 2), "numeric", "text"))
head(coho_weir)
tail(coho_weir)
lapply(coho_weir, table, useNA = "ifany")
#Only 4 strata
#No samples in first strata
table(coho_weir$samples == 0, coho_weir$strata, useNA = "ifany")
range(table(coho_weir$date))

#Looks like Weir was pulled early
library(ggplot2)
ggplot(coho_weir, aes(x = date, weight = passage)) + 
  geom_bar() +
  ggtitle("Deshka Coho Escapement") +
  labs(x = "Date", y = "Coho Salmon")

coho_weir_strata <-
  coho_weir %>%
  dplyr::group_by(strata) %>%
  dplyr::summarise(total = sum(passage),
                   samples = sum(samples))
coho_weir_sum <- dplyr::summarise(coho_weir_strata, total = sum (total))

coho_sl <- 
  readxl::read_xlsx(".\\2021\\2021 Deshka coho sex comp_DL.xlsx",
                    sheet = 4,
                    range = "A5:F178",
                    col_names = c("date", "sex", "length", "strata"),
                    col_types = c("date", rep("skip", 2), "text", "numeric", "text"))
head(coho_sl)
tail(coho_sl)
#0 fish from 1st strata, 7 fish from second strata 
hist(coho_sl$date, breaks = "days", freq = TRUE, format = "%b%d")
hist(coho_sl$length)
lapply(coho_sl, table, useNA = "ifany")
coho_sl <- coho_sl[!is.na(coho_sl$sex), ] #drop fish that wasnt sampled

#No samples from 1 strata, 7 fish from second strata.
#Matches weir strata.
table(coho_sl$date, coho_sl$strata, useNA = "ifany") 

#Not expecting sampling in proportion to passage
plot_ks(coho_weir, "passage")

#sex comp time invariant
#test useless since we have no infor on 1/3 of the run
tab_lr(coho_sl, "sex")

#Produced a pooled age comp without apportioning the run becasue:
#1) the weir was pulled early and
#2) the run was sampled for just a few days
pooled_sl <- asl(coho_sl) %>%
  tab_asl(totalname = "Weir Passage", output = "sl")
pooled_sl


