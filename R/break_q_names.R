#'@title add line break to question names 
#'
#'@description add line breaks to question names - split into two lines.
#'Line lengths are determined by the longest question.
#'
#'@param q_names question names - character
#'@param max_lines maximum number of lines. Int, defaults to 2
#'@export

break_q_names <- function(q_names, max_lines = 2) {
  
  # Validate input
  if (typeof(q_names) != "character") {
    stop("Unexpected input - q_names is not a character vector")
  } else if (!is.numeric(max_lines) | length(max_lines) > 1) {
    stop("Unexpected input - max_lines is not a single integer")
  }
  
  max_length = max(nchar(q_names)) / (max_lines-1)
  wrap_loc = ceiling(max_length / (2))
  
  regex <- paste0("(.{", wrap_loc, "}.?) (.?)")
  wrapped_strings <- gsub(regex , "\\1<br>\\2", q_names)
  
  return(wrapped_strings)
}