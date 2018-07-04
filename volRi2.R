library(dplyr)
library(quantmod)
library(rusquant)
library(lubridate)
library(e1071)

# Загрузка дневных цен на индекс РТС с 2013 г.
RTS <- getSymbols("SPFB.RTS", 
           src = "Finam", 
           from = "2013-01-01",
           period = "day",
           auto.assign = FALSE)

# Расчет волатильности по методу Янг Жанг
volRi2 <- RTS %>% 
  volatility(n = 2, 
             N = 252, 
             calc = "yang.zhang") %>% 
  .[!is.na(.)]

# Находим день недели для каждой даты
dayOfWeek <- volRi2 %>% 
  lubridate::wday(label = TRUE)

# Разбиваем данные на недели
split_df <- split(volRi2, f = "weeks")

# Из каждого значения волатильности вычитаем среднее за соответствующую неделю
diffMeanList <- lapply(split_df, function(x) x - mean(x))
volRi2_merged <- merge(volRi2, diffMeanList = unlist(diffMeanList))

# Бинарная классификация: волатильность выше средней - "UP", ниже - "DOWN"
Class <- ifelse(volRi2_merged$diffMeanList > 0 ,"UP","DOWN")

# Таблица классов по дням недели
classOfDay <- data.frame(dayOfWeek,Class)

# Модель наивного байесовского классификатора
naiveModel <- naiveBayes(classOfDay[,1],classOfDay[,2])

# Предсказание условной вероятности движения цены в зависимости от дня недели
naiveModel

# Таблица количества движения цены вверх/вниз по дням
table(classOfDay)



