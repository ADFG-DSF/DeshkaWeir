# Find Dates that satisfy Strata requirements #

#### Data Import ####
weir <- read_csv("./2023/data/2023 Deshka weir.csv",
                 col_types = cols(date = col_date(format = "%Y-%m-%d")))

asl_dat <- read_csv("./2023/data/2023 Deshka asl.csv",
                    col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                     age = col_character()))
#### 

# Assign numeric values to dates, easier to work with than actual date format. Ex: Day 1 = 6/1/2023
weir$day <- seq(1:length(weir$date))

# Creates a matrix of possible breaks for strata designations. Truncated a bit to keep the matrix relatively small
#Used realistic values. At day 33, 90% passage and 89% sampled.Using higher values caused the difference calc
# to be unrealistic due to 100% in strata 4
break1 = seq(10,23,1)
break2 = seq(14,32,1)
break3 = seq(15,33,1)

breaks = expand_grid(break1,break2,break3) %>% 
  filter(break3>break2,break2>break1)
#

# Create an empty data frame to fill with dates and variance values

summary <- data.frame(break1 = numeric(0), 
                      break2 = numeric(0), 
                      break3 = numeric(0), 
                      passage_ssd = numeric(0), 
                      samples_ssd = numeric(0), 
                      comb_ssd = numeric(0))
summary <- matrix(NA,nrow=nrow(breaks),ncol=7)
colnames(summary) =  c("break1", "break2", "break3", "passage_ssd", "samples_ssd", "comb_ssd","diff")

#### Creates for loop that tests all possible strata combinations and calculates the ssq diff to 25% 
#for each strata for passage and samples 

for (i in 1:nrow(breaks)) {
  
  cut <- breaks[i,]
  
  weir <- weir %>%
    mutate(
      strata = case_when(
        day <= cut$break1 ~ 1,
        day > cut$break1 & day <= cut$break2 ~ 2,
        day > cut$break2 & day <= cut$break3 ~ 3,
        TRUE ~ 4
      )
    )
  
  weir_summary <- weir %>%
    group_by(strata) %>%
    summarize(
      passage_percent = sum(passage) / sum(weir$passage),
      samples_percent = sum(samples) / sum(weir$samples)
    )
  
  passage_ssd <- sum((weir_summary$passage_percent - 0.25)^2)
  samples_ssd <- sum((weir_summary$samples_percent - 0.25)^2)
  comb_ssd <- passage_ssd + samples_ssd
  diff <- sum((weir_summary$passage_percent-weir_summary$samples_percent)^2)
  
  # Append a new row to the summary data frame
  
  summary[i,] <- c(cut$break1, cut$break2, cut$break3, passage_ssd, samples_ssd, comb_ssd,diff)
}

summary = as_tibble(summary)
####

#### Extract best values

comb_min = summary[which.min(summary$comb_ssd),] 
pass_min = summary[which.min(summary$passage_ssd),] 
samp_min = summary[which.min(summary$samples_ssd),]
diff_min = summary[which.min(summary$diff),]


summary$scaled_pass <- scale(summary$passage_ssd)
summary$scaled_samp <- scale(summary$samples_ssd)
summary$scaled_comb <- scale(summary$comb_ssd)
summary$scaled_diff <- scale(summary$diff)
summary$scaled_all <- scale(summary$passage_ssd)+scale(summary$samples_ssd)+scale(summary$comb_ssd)+scale(summary$diff)

summary[which.min(summary$scaled_all),]
view(summary)
