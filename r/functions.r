load_data <- function(file = 'data/tele2.csv') {
  library(dplyr)
  library(stringr)
  library(lubridate)
  data <- read.table(file, header=T, sep=';', quote='"',
                     stringsAsFactors=F)
  data <- data[, names(data)!='X']
  data$date <- with(data, str_c(date, time, sep=' '))
  data$date <- dmy_hms(data$date)
  data <- data[, !(names(data) %in% c('minutes', 'time'))]
  data <- data %>%
    mutate(day = floor_date(date, 'day'),
           
           # Need new var with type of call recipient (our network or foreign)
           network = ifelse(
             str_detect(type, perl('Исходящий на (МТС$|Мегафон$|Билайн$)')),
             "чужой", ifelse(
               str_detect(type, 'Исходящий на своего'), "свой", "прочее"))) %>%
    mutate(minutes = ceiling_mins(seconds, 3),
           bytes = get_bytes(number)) %>%
    group_by(day, network) %>%
    
    # Cumsum of minutes of calls to foreign networks is requiried 
    mutate(mcumsum = cumsum(minutes))
  data}

isout <- function(call) {
  library(stringr)
  str_detect(call, "^Исходящий")
}

ceiling_mins <- function(x, limit) {
  ifelse(x > limit, ceiling(x/60), 0)}

# Add parentheses to R-expressions of tariffs elements
add_arg <- function(arg, fun) {
  if(length(arg) > 1) return(unlist(lapply(arg, add_arg, fun)))
  paste(fun, "(", arg, ")", sep="")
}

# How much is does cost element of calling history in condition
# of a tariff plan
el_pay <- function(row_numb, data, plan, options) {
  x <- data[row_numb,, drop=F]
  match_rule <- function(cond, r) {
    if(length(cond) > 1) return(unlist(lapply(cond, match_rule, r)))
    eval(parse(text=cond)[[1]], envir=r)
    
  }
  x_rule <- match_rule(plan$cond, x)
  if(sum(x_rule, na.rm=T) == 0) stop(paste('Row ', row_numb, 
                                  " doesn't match any condition", sep=""))
  if(sum(x_rule, na.rm=T) > 1) stop(paste('Row ', row_numb, 
                                 " matches more than one condition", sep=""))
  
  if(!missing(options) & dim(optins)[1] > 0) {
    opt_rule <- match_rule(options$cond, x)
    if(sum(opt_rule, na.rm=T) > 1) 
      stop(paste('Row ', row_numb, 
                 " matches more than one option", sep=""))
    
    if(sum(opt_rule, na.rm=T) == 1) 
      return(eval(parse(text=options$rate[opt_rule])[[1]], envir=x))
  }
  
  pay <- eval(parse(text=plan$rate[x_rule])[[1]], envir=x)
  return(pay)
}

# Randomzing of phone numbers

rnd_phone <- function(phone) {
  if(length(phone) > 1) return(unlist(lapply(phone, rnd_phone)))
  require(stringr)
  phone_reg <- "(^\\+[1-9][0-9]{3})([0-9]*)"
  phone_len <- str_length(phone) - 5
  ifelse(str_detect(phone, phone_reg),
         # Regular phone numbers
         str_replace(phone, phone_reg, 
                     str_c('\\1', round(runif(1, 10^(phone_len - 1),
                                              10^phone_len -1), 0))),
         # Short phone numbers
         ifelse(str_detect(phone, "^[0-9]{3,}"), 
                round(runif(1, 100, 9999), 0), phone))
}

get_bytes <- function(phone) {
  if(length(phone) > 1) return(unlist(lapply(phone, get_bytes)))
  require(stringr)
  if(!str_detect(phone, "^Трафик: ")) return(NA)
  as.numeric(str_replace(phone, "(^Трафик: )([1-9][0-9]*)( байт$)", "\\2"))
}

load_plans <- function(file = "data/plan.csv") {
  require(stringr)
  plan <- read.table(file, header = T, sep = ";", stringsAsFactors = F)
  plan <- plan[nchar(plan$cond) > 0,]
  plan$cond <- str_replace_all(plan$cond, '\\n', '')
  plan$rate <- str_replace_all(plan$rate, '\\n', '')
  plan$rate <- ifelse(plan$plantype != 'option', 
                      str_c("modelsum = ", plan$rate),
                      str_c("modelpay = ", plan$rate))
  plan
}


# Generate list of all possible plans
get_plans <- function(plans) {
  require(dplyr)
  roaming <- plans[plans$plantype == "roaming",]
  plans %>% 
    filter(plantype == "basic") %>%
    group_by(plan) %>%
    do(planset = rbind(., roaming))
          
}

# Stuff for bootstrap

get_pays <- function(data, plan, options) {
  unlist(lapply(seq_len(dim(data)[1]), 
                el_pay, 
                data = data, 
                plan = plan,
                options = options))
}

get_boot <- function(d, indices) {
  d <- d[indices]
  sum(d)
}

get_boot_bill <- function(plan, data, ...) {
  pays_orig <- get_pays(data = data, plan = plan)
  boot(data = pays_orig, statistic = get_boot, ...)
}

# Combinations of options

opts_combs <- function(optsn) {
  opts_combn <- 2 ^ optsn
  lapply(seq_len(opts_combn), function(x) {
    x <- intToBits(x)
    x <- as.integer(x)
    x <- x[seq_len(optsn)]
    as.logical(x)
  })
}