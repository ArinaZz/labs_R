get_count_hospitals_location <- function(location, data) {
  location_id <- data[data$City == location, "Facility.Name"]
  if (length(location_id) == 0) {
    location_id <- data[data$State == location, "Facility.Name"]
  }
  if (length(location_id) == 0) {
    location_id <- data[data$County.Name == location, "Facility.Name"]
  }
  
  # Выводим ошибку если не нашли совпадений
  if (length(location_id) == 0) {
    return("Error! You write an incorrect location!")
  }
  
  quantity_of_hospitals <- list()
  quantity_of_hospitals <- length(unique(location_id))
  
  return(quantity_of_hospitals)
}

file <- "Payment_and_Value_of_Care-Hospital.csv"
data <- read.csv(file)

# Проверка города, штата, округа и неккоректного ввода
get_count_hospitals_location('FLORENCE', data)
get_count_hospitals_location('AL', data)
get_count_hospitals_location('CAMDEN', data)
get_count_hospitals_location('Non-existent place', data)

