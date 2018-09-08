library(highfrequency)
library(rusquant)
library(dplyr)

si <- getSymbols("SPFB.Si", src = "Finam", 
                 from = "2018-01-03",to = "2018-03-25",
                 period = "1min",auto.assign = FALSE)

# split by days
spl <- si %>% split(f = "days", k=1)

# aggregate each element of list on 5 min
# NOTE(!): Periodicity estimation requires at least 50 observations (length(arg) > 50)
agr <- spl %>% lapply(function(x) aggregatePrice(x$SPFB.SI.Close,on = "minutes",k = 5,
                                                 marketopen = "10:00:00", 
                                                 marketclose = "23:45:00",
                                                 tz = "Europe/Moscow"))

# create a matrix
mat <- matrix(unlist(agr), nrow = length(agr[[1]]))

# estimating spot volaitility
sp <- t(mat) %>% spotvol(method="detper")

plot(sp)

# ==========
# to check if length of all elements of list agr are equal
length(unique(lengths(agr))) == 1L
lapply(agr, function(x) length(x) == length(agr[[1]])) %>% melt(.)
