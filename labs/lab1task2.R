merge_dataframe_list <- function(all_data) {
  return(Reduce(function(x, y) merge(x, y, by = 'id'), all_data))
}


get_id <- function(dfs) {
  merged = merge_dataframe_list(all_data)
  measurements = merged[-which(colnames(merged)=='id')]
  means = rowMeans(measurements)
  return(data.frame(id=merged$id, mean_temp=means))
}

load("lab1_e2.RData")
get_id(data)