fix_data <- function(df) {
  
  apply_to_column <- function(original_column) {
    replaced_column <- sub(' ', '', original_column)
    numeric_column <- as.numeric(replaced_column)
    
    if (any(is.na(numeric_column))) {
      return(original_column)
    }
    
    return(numeric_column)
  }
  
  return(data.frame(lapply(df, apply_to_column)))
}

data <- read.csv("lab1_e1.csv")
fix_data(data)