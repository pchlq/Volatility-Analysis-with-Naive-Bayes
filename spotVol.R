library(highfrequency)
library(rusquant)
library(dplyr)
library(PerformanceAnalytics)

options(scipen = 10)

ts <- getSymbols("SPFB.Si", src = "Finam", 
                 from = "2018-06-29",to = "2018-09-14",
                 period = "1min",auto.assign = FALSE)


ts.agr <- ts %>% Cl() %>% CalculateReturns() %>% na.locf(fromLast = TRUE) %>% # get returns of close price
              split(f = "days", k = 1) %>% # split by days
              
              # aggregate each element of list on 5 min
              # NOTE(!): Periodicity estimation requires at least 50 observations (length(arg) > 50)
              lapply(function(x) aggregatePrice(x,on = "minutes",k = 5,
                                                marketopen  = "10:00:00", 
                                                marketclose = "23:45:00",
                                                tz   = "Europe/Moscow"))
             

sp <- matrix(unlist(ts.agr), nrow = length(ts.agr[[1]])) %>% # creating a matrix
  t() %>% spotvol(method = "detper") # estimating spot volaitility

# plot
plot(sp)

# ==========
# to check if length of all elements of list agr are equal
length(unique(lengths(ts.agr))) == 1L
lapply(ts.agr, function(x) length(x) == length(ts.agr[[1]])) %>% melt()
