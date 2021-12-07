#' @title Generate dummy data similar to carsurvey wave 2 responses
#'
#' @details Generates non-disclosure data similar to the carsurvey wave 2 responses. 
#' The column names are the same, same number of column. The data is similar but the distbutions are random.
#'  The disrbutions are NOT the same, e.g for department this is sampled randomly so each deartment has a equal chance of being selected.
#'
#' @param nrows Number of rows to generate
#'
#' @return data.frame
#' 
#' @export

generate_dummy_data <- function(nrows = 100) {
  
  API_data <- carsurvey2::ingest()
  if(API_data$status_code != 200) stop("Unsuccessful API request. Status code: ", API_data$status_code, "\n Process Killed.")
  
  data <- carsurvey2::tidy_ingest(carsurvey2::convert_raw(API_data))
      
  cols <- colnames(data)
  vals <- lapply(cols, function(x) extract_vals(data, x))
  
  dummy_data <- data.frame(lapply(vals, function(x) sample_vals(x, nrows)))
  
  colnames(dummy_data) <- cols
  
  return(dummy_data)
  
}

extract_vals <- function(data, col_name) {
  
  counts <- data.frame(table(data[col_name]))
  
  if (nrow(counts) == 0) {
    return("empty")
  }
  
  counts <- counts[counts[[2]] > 20, ]
  
  if (nrow(counts) == 0) {
    return("text")  
  } else {
    return(counts[[1]])
  }
}

sample_vals <- function(vals, n_rows) {
  
  if (vals == "text") {
    return(sample(lexicon::cliches, n_rows, replace=TRUE))
  } else if (vals == "empty") {
    return(rep(NA, n_rows))
  } else {
    set.seed(c(1:n_rows))
    vals <- as.character(vals)
    return(sample(vals, n_rows, replace=TRUE))
  }
  
}


