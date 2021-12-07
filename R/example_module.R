#'@title fit logistic regression
#'
#'@description fit logistic regression onto data
#'
#'@param formula string. R formula as character string.
#'@param data data.frame. 
#'
#'@return model (gml)
#'
#'@export

fit_log_reg <- function(formula, data) {
  model <- stats::glm(stats::as.formula(formula), family = "binomial", data = data)
  
  return(model)
}

