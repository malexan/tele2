# library(devtools)
# dev_mode(on=F)
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
source('r/functions.r')
library(boot)
# library(doParallel)
# registerDoParallel(cores=2)
# library(parallel)

data <- load_data()
plans <- load_plans()
plans <- get_plans(plans)



system.time(rez_list <- 
              lapply(plans$planset, get_boot_bill, data = data, R = 5000))

df <- 

library(ggplot2)
rez_list[[1]]$t

ggplot(data.frame(x = rez_list[[1]]$t), aes(x)) + geom_density()

