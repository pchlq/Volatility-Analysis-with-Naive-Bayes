library(dplyr)
library(rusquant)
library(spikeslab)
library(quantmod)
library(xts)
library(stringr)
library(purrr)


system.time(vola_prediction(asset = "SPFB.SI",
                            tikers = c("CBOT.TY", "RGBI", "VIX", "SPFB.Si"),
                            dateFrome = "2017-01-01",
                            estimators = c("yang.zhang", "close"),
                            volaPeriod = c(3, 5, 10, 60),
                            Y = "yang.zhang_5",
                            nLags = 5:10,
                            daysPredict = 5))


vola_prediction <-  function(asset, tikers, dateFrome, dateEnd = Sys.Date(), periodTime = "day", 
                             estimators, volaPeriod, periodYear = 252, 
                             Y, nLags, ssIter = 4000, daysPredict){
  
  # 1. import =====================================
  if ("VIX" %in% tikers){
    noVIX <- tikers[!tikers %in% "VIX"]
    for (i in (1:length(noVIX))){
      getSymbols(noVIX[i], src="Finam", from = dateFrome, period = periodTime)
      Sys.sleep(5)
    }
    getSymbols("^VIX", src = "yahoo", from = dateFrome,  period = periodTime)
  } else {
    for (i in (1:length(tikers))){
      getSymbols(tikers[i], src="Finam", from = dateFrome, period = periodTime)
      Sys.sleep(5)
    }
  }
  
  up_tikers <- lapply(tikers, function(x) toupper(x)) %>% unlist(.)
  
  # 2. Joint Spredsheet: js =====================================
  tikers_df <- do.call(merge, lapply(up_tikers[!up_tikers %in% asset], function(x) Cl(get(x)))) %>% 
    setNames(up_tikers[!up_tikers %in% asset]) %>% 
    na.locf(.)
  
  vola <- function(n, estimators){
    volatility(get(asset), n, N = periodYear, calc = estimators)
  }
  
  volaName <- function(estimators) volaPeriod %>% 
    map(~ vola(.x, estimators)) %>% 
    as.data.frame(.) %>% 
    setNames(., paste(estimators, volaPeriod, sep="_"))
  
  vol_df <- map(estimators, volaName) %>% as.data.frame(.) %>% as.xts(.)
  index(vol_df) <- as.Date(index(vol_df))
  js <- merge(tikers_df, vol_df) %>% na.locf(.)
  startDate <- lapply(js, function(x) start(x[!is.na(x)])) %>% melt(.) %>% .$value %>% max(.)
  
  js <- js[paste(startDate,dateEnd, sep = "/")]
  
  
  ## 3. spike and slab regression =====================================
  lgNames <- names(js)
  vecLags <- nLags %>% 
    map(~paste0("lag(",lgNames,", ", .x,")", sep = "")) %>% unlist(.)
  ss_formula <- as.formula(paste(Y, "~ ", paste(vecLags, collapse= "+")))  
  ss_model <- spikeslab(ss_formula, data = js, n.iter2 = ssIter)
  
  included_regressors <- melt(ss_model$model)
  
  top_regressors <- table(ss_model$names[included_regressors$value]) %>% 
    melt(.) %>% 
    arrange(desc(.$value)) %>% 
    mutate(prob = round((.$value / ssIter * 100), 5)) %>% 
    .[seq(ss_model$phat),] %>% print(.)
  
  
  ## 4. linear regression model & predict =====================================
  dr <- droplevels(top_regressors)$Var1 %>% levels(.)
  
  lm_formula <- as.formula(paste(Y ," ~ ", paste(dr, collapse= "+")))
  lm_model <- lm(formula = lm_formula, data = js)
  
  # empty matrix
  m <- matrix(0, daysPredict, dim(js)[2])
  d <- seq(as.Date(dateEnd)+1, length = nrow(m), by = "days")
  m.xts <- xts(m, order.by=d)
  
  df_predict <- rbind(js, m.xts)
  
  predict_model <- predict(lm_model, df_predict, interval = "prediction", vcov. = vcovHAC(lm_model))
  
  print(ss_model)
  print(tail(predict_model, daysPredict))
  
}
