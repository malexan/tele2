library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
library(boot)
library(ggplot2)
source('r/functions.r')

# library(doParallel)
# registerDoParallel(cores=2)
# library(parallel)

data <- load_data()
plans <- load_plans()



# Options. 3 options mean 2^3 of their combinations
options <- plans %>%
  filter (plantype == 'option') %>%
  group_by(plan)




# Convert data.frame with tariff plan data
# to list of tariffs
plans <- get_plans(plans)




rez_list <- lapply(plans$planset, get_boot_bill, data = data, R = 5000)

# ggplot(data.frame(x = rez_list[[1]]$t), aes(x)) + geom_density()
