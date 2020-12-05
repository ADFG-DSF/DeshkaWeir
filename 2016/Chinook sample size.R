#Chinook sample size
d2n <- 1.27359 #Thompson 1987 Table 1, alpha = 0.05
n <- d2n/c(.07, .075, .08)^2 # d 
na <- n/0.8 # 75% regenerated scales
na #required sample size

#Chinook sample rate
13000/325

#Coho % past weir (12-15)
cum <-c(10842, 11578, 22141, 6825)
j15 <- c(2, 93, 28, 2)
a27 <- c(9536, 10430, 22008, 6628)
pct <- 1-(cum-a27+j15)/cum; pct
