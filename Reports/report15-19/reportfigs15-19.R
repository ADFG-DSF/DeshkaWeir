library(magrittr)
library(ggplot2)
dodge <- position_dodge(width=0.9)
readxl::read_excel("H:\\My Documents\\DeshkaWeir\\Reports\\report15-19\\Copy of Deshka Tbls_Figs_Apns__15-19_AR.xlsx", 
                   sheet = "coho sex", 
                   range = "A5:G15") %>%
  dplyr::rename(pct = '%', se = SE, lci = LCI, uci = UCI, n = 'Sample Size') %>%
  tidyr::fill(Year, .direction = "down") %>%
  ggplot(aes(x = Year, y = pct, fill = Sex))  +
    geom_col(position = dodge) +
    geom_errorbar(aes(ymin = lci, ymax = uci), position = dodge, width = 0.25) +
    scale_y_continuous(name = "Percentage of Run", limits = c(0, 100)) +
    scale_fill_discrete(name = "") +
    theme_bw(base_size = 18) +
  ggtitle("Deshka River Coho Salmon Sex Composition")

readxl::read_excel("H:\\My Documents\\DeshkaWeir\\Reports\\Copy of Deshka Tbls_Figs_Apns__15-19_AR.xlsx", 
                   sheet = "Tab6 KS sex comp", 
                   range = "A3:G13") %>%
  dplyr::rename(pct = '%', se = SE, lci = LCI, uci = UCI, n = 'Sample Size') %>%
  tidyr::fill(Year, .direction = "down") %>%
  ggplot(aes(x = Year, y = pct, fill = Sex))  +
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = lci, ymax = uci), position = dodge, width = 0.25) +
  scale_y_continuous(name = "Percentage of Run", limits = c(0, 100)) +
  scale_fill_discrete(name = "") +
  theme_bw(base_size = 18) +
  ggtitle("Deshka River Chinook Salmon Sex Composition")

readxl::read_excel("H:\\My Documents\\DeshkaWeir\\Reports\\Copy of Deshka Tbls_Figs_Apns__15-19_AR.xlsx", 
                   sheet = "Tab 5", 
                   range = "A7:N23",
                   col_names = c("sex", "year", paste0(c("p_", "se_", "skip"), rep(1:4, each = 3))),
                   col_types = c("text", "numeric", rep(c("numeric", "numeric", "skip"), times = 4))) %>%
  dplyr::filter(!is.na(year)) %>%
  tidyr::fill(sex, .direction = "down") %>%
  tidyr::pivot_longer(tidyselect::starts_with(c("p_", "se_")), names_to = c("stat", "age"), names_sep = "_") %>%
  tidyr::pivot_wider(id_cols = c("sex", "year", "age"), names_from = "stat") %>%
  dplyr::mutate(sex = factor(sex, levels = c("Male", "Female", "Combined")),
                age = factor(age, levels = 1:4, labels = c(paste0("Age 1.", 1:4))),
                logitlci_p = log(p/100 / (1-p/100)) - 1.96 * sqrt(1 / (p/100)^2 / (1-p/100)^2 * (se/100)^2),
                logituci_p = log(p/100 / (1-p/100)) + 1.96 * sqrt(1 / (p/100)^2 / (1-p/100)^2 * (se/100)^2),
                lci = exp(logitlci_p)/(1 + exp(logitlci_p)) * 100,
                uci = exp(logituci_p)/(1 + exp(logituci_p)) * 100) %>%
  ggplot(aes(x = year, y = p, fill = age))  +
    geom_col(position = dodge) +
    geom_errorbar(aes(ymin = lci, ymax = uci), position = dodge, width = 0.25) +
  scale_y_continuous(name = "Percentage of Run", limits = c(0, 100)) +
  scale_fill_discrete(name = "") +
  xlab("Year") +
  facet_grid(sex~.) +
  theme_bw(base_size = 18) +
  ggtitle("Deshka River Chinook Salmon Age Composition")


coho_rt <-
  readxl::read_excel("H:\\My Documents\\DeshkaWeir\\Reports\\Report15-19\\Copy of Deshka Tbls_Figs_Apns__15-19_AR.xlsx", 
                   sheet = "App B Coho Run Timing", 
                   range = "A5:AT79",
                   col_names = c("date", as.character(2005:2019)),
                   col_types = c("date", rep("skip", 30), rep("numeric", 15))) %>%
  tidyr::pivot_longer(cols = -"date", names_to = "year") %>%
  dplyr::mutate(month = format(date, "%b"),
                day = format(date, "%d"),
                date = as.Date(paste0(month, "-", day, "-", year), "%b-%d-%Y"),
                jd = format(date, "%j"))

flood1 <- 
  coho_rt$value[coho_rt$jd == format(as.Date("Aug-12-2016", "%b-%d-%Y"), "%j") & !(coho_rt$year %in% c("2011", "2016"))] -
    coho_rt$value[coho_rt$jd == format(as.Date("Aug-08-2016", "%b-%d-%Y"), "%j") & !(coho_rt$year %in% c("2011", "2016"))]
hist(flood1, seq(0, 0.6, 0.05))
abline(v = coho_rt$value[coho_rt$jd == format(as.Date("Aug-12-2016", "%b-%d-%Y"), "%j") & coho_rt$year == "2016"] -
         coho_rt$value[coho_rt$jd == format(as.Date("Aug-08-2016", "%b-%d-%Y"), "%j") & coho_rt$year == "2016"])

flood2 <- 
  coho_rt$value[coho_rt$jd == format(as.Date("Aug-28-2016", "%b-%d-%Y"), "%j") & !(coho_rt$year %in% c("2011", "2016"))] -
  coho_rt$value[coho_rt$jd == format(as.Date("Aug-24-2016", "%b-%d-%Y"), "%j") & !(coho_rt$year %in% c("2011", "2016"))]
hist(flood2, seq(0, 0.2, 0.025))
abline(v = coho_rt$value[coho_rt$jd == format(as.Date("Aug-28-2016", "%b-%d-%Y"), "%j") & coho_rt$year == "2016"] -
         coho_rt$value[coho_rt$jd == format(as.Date("Aug-24-2016", "%b-%d-%Y"), "%j") & coho_rt$year == "2016"])
