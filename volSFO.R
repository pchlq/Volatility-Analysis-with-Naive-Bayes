library(dplyr)
library(rusquant)

Tickers <- c("SPFB.ED", "SPFB.VTBR", "ALRS", "RUAL", "RTKM", "PHOR", "RSTIP", "SPFB.GAZR")

volSFO <- function(tickers, volaPeriod, periodPerYear, dateFrom, periodTime){
  #' @param tickers vector of assets
  #' @param volaPeriod integer. Number of periods for the volatility estimate
  #' @param periodPerYear integer or float. Number of periods per year
  #' @param dateFrom date. Date start
  #' @param periodTime ("hour", "day", "week" etc.
  #' @return result datatable 
  #' 
  #' @examples
  #' volSFO(c("ALRS", "RUAL"), n=4, N=52.4, dateFrome="2018-01-01", periodTime="day"))
  
  if ( ! exists(tickers) ) {
    for (i in (1:length(tickers))){
      getSymbols(tickers[i], src="Finam", from = dateFrom, period = periodTime)
      Sys.sleep(10)
    }
  }
  
  vola <- function(asset, estimator){
    #' Computes average volatility 
    volatility(asset, n = volaPeriod, N = periodPerYear, calc = deparse(substitute(estimator))) %>%
      na.omit(.) %>% 
      mean(.)
  }
  
  result <- data.frame(VolaSFOpc = sapply(tickers, function(x) vola(get(x), parkinson)),
                       
                       VolaSFOpl = sapply(tickers, function(x) vola(get(x), parkinson)) /
                         sapply(tickers, function(x) vola(get(x), close)))
  
  return(result)
}

volSFO <- Tickers %>% 
             volSFO(volaPeriod = 4, periodPerYear = 52.4, 
                    dateFrom = "2013-01-01", 
                    periodTime = "week")
