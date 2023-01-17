getmaxAvg_week <- function (data) {
  max_Temp <- -220
  hottest_Week <- -1
  temp_count <- 0
  week_count <- 0
  for (row in seq_len(nrow(data))) {

    # Переводим значение даты из таблицы в словесный формат
    day_Week <- weekdays(as.Date(data[row, "YYYYMMDD"]))
    temp_count <- temp_count + data[row, "T2M"]
    
    # Определяем начало недели
    if (day_Week == "понедельник" ) {
      if (hottest_Week == -1) hottest_Week <- 1
      else {
        
        # Сравниваем максимальное среднее с получившемся текущим средним
        if (max_Temp < temp_count/7) {
          hottest_Week <- week_count
          max_Temp <- temp_count/7
        }
        temp_count <- 0
      }
      week_count <- week_count + 1
    }
  }
  return(hottest_Week)
}

file <- "RH_T.csv"
data <- read.csv(file)
getmaxAvg_week(data)