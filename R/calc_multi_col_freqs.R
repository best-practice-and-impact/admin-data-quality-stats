#'@title Calculate frequencies for multiple columns 
#'
#'@description Calculates frequencies for multiple dataframe columns and returns them in a single data frame which can be used with the 
#'plot_stacked or plot_likert functions
#'
#'@param cols the selected columns (data.frame)
#'@param factor_levels a list of factor levels for the cell values of all columns
#'@param calc_props whether or not to calculate proportions (logical, FALSE by default)
#'
#'@return frequencies data frame with the frequencies of response options by column
#'
#'@export

calc_multi_col_freqs <- function(cols, factor_levels, calc_props = FALSE){
  
  # Validate inputs
  if (class(cols) != "data.frame") {
    stop("Unexpected input - cols is not a data.frame")
  } else if (typeof(factor_levels) != "character") {
    stop("Unexpected input - factor_level is not a character vector")
  } else if (typeof(calc_props) != "logical" | length(calc_props) > 1) {
    stop("Unexpected input - calc_percentages is not a single logical value")
  }
  
  data <- data.frame(lapply(cols, function(x) factor(x, levels = factor_levels)))
  data <- data.frame(sapply(data, table))
  data <- data.frame(t(data))
  
  if (calc_props) {
    data <- data/rowSums(data)
  }
  
  data <- cbind(question = rownames(data), data)
  rownames(data) = NULL
  
  return(data)
}
