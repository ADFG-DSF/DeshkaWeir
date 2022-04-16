################################# Calculate prediction intervals ##################################################
############################## Carefull!!!!! modify this before you run it so you don't overwrite dat objects #######
library(magrittr)
library(preseason)
rm(list = ls())


#get data
load("H:\\My Documents\\DeshkaWeir\\2022\\age4.rda")
load("H:\\My Documents\\DeshkaWeir\\2022\\age5.rda")
load("H:\\My Documents\\DeshkaWeir\\2022\\age6.rda")

#caluclate return year and prep data

dat4$ryr <- dat4$byr + 4
dat5$ryr <- dat5$byr + 5
dat6$ryr <- dat6$byr + 6

dat4CI <- dat4[dat4$ryr %in% 1985:2021, c("ryr", "age4", "md5_pred")] # I modifed this to jest filter out the NAs
dat5CI <- dat5[dat5$ryr %in% 1985:2021, c("ryr", "age5", "sibmd_45pred")]
dat6CI <- dat6[dat6$ryr %in% 1985:2021, c("ryr", "age6", "es_pred")]

#calculate errors
errors <- 
  dplyr::left_join(dat4CI, dat5CI, "ryr") %>%
  dplyr::left_join(dat6CI, "ryr") %>%
  dplyr::mutate(return = age4 + age5 + age6,
                hind = md5_pred + sibmd_45pred + es_pred,
                error = hind - return,
                error_ln = log(hind/return)) #errors on the log(prediction) - log(return) 
hist(errors$return)
hist(log(errors$return))
hist(errors$hind)
hist(log(errors$hind))

hist(errors$error)
hist(errors$error_ln)

mean(errors$error_ln) #slight bias in our predictions

#rmse 80% bounds (log scale)
#I'm not sure which one to use.
#The first one uses only the years we used to do model selction which makes some sense.
#the second one is more robust. I guess I prefer it.
exp(sqrt(mean(errors$error_ln[33:37]^2)) * c(qt(.1, 5), qt(.9, 5)))  * 9332 
exp(sqrt(mean(errors$error_ln^2)) * c(qt(.1, 37), qt(.9, 37))) * 9332

#Here is the com models code w mape added in.
comp_models2 <- function(dat, comp_age, years = 5){
  age <- paste0("age", comp_age)
  diff <- dat %>%
    dplyr::select(byr, !!age, dplyr::ends_with("pred")) %>%
    tidyr::gather(type, pred, -byr, -!!age) %>%
    dplyr::mutate(d = pred - (!!as.name(age)),
                  pe = d / (!!as.name(age)))
  
  plot <- ggplot2::ggplot(diff, ggplot2::aes(x = byr, y = pred, color = type)) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
    ggplot2::geom_point(ggplot2::aes_string(y = age), color = "black") +
    ggplot2::geom_line(ggplot2::aes_string(y = age), color = "black")
  
  table <- diff %>%
    dplyr::group_by(type) %>%
    dplyr::top_n(years, byr) %>%
    dplyr::summarise(md = mean(d),
                     mad = mean(abs(d)),
                     mape = mean(abs(pe)))
  
  list(plot, table)
}

#No changes to 4 and 5. Slight change to 6.
comp_models2(dat4, 4)
comp_models2(dat5, 5)
comp_models2(dat6, 6)

#####Warning purley pedagogical#######
#Here are the errors that were averaged to get the MAD and MAPE for age 6 fish.
#Notice that the error in 2011 is similar in size to the error in 2012 in absolute terms but very different in percentage terms  
dat6 %>%
  dplyr::select(byr, age6, dplyr::ends_with("pred")) %>%
  tidyr::gather(type, pred, -byr, -age6) %>%
  dplyr::mutate(d = pred - age6, pe = d / age6) %>% 
  dplyr::group_by(type) %>%
  dplyr::top_n(5, byr) %>% 
  tidyr::pivot_longer(d:pe, names_to = "error", values_to = "val") %>% 
  dplyr::filter(type %in% c("es_pred", "md_pred")) %>%
  dplyr::arrange(type, error, byr) %>%
  print(n = 40) %>%
    ggplot2::ggplot(ggplot2::aes(x = byr, y = val, color = type)) + 
    ggplot2::geom_line() + 
    ggplot2::facet_grid(error ~ ., scales = "free_y")

#Notice that to have gotten the same size MAPE (for the es model) for the 2011 return would have taken a forecast of 
1299 *(15.9 + 1)
#also note that if 0 age 6 fish were counted the MAPE will not work.
#also note it is impossible to get a MAPE of over 1 for an underforecast.
#Lastly you could modify MAD to get something on a percentage scale (read this online... have not tried)
# MAD% = mean(abs(pred - actual)) / mean(actual) = sum(abs(pred - actual)) / sum(actual)


