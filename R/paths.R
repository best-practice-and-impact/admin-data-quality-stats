
#' @title Format filter path
#'
#' @description This function cleans the filter names
#'
#' @param path A string 
#'
#' @return String
#' 
#' @export
#'

format_filter_path <- function(path) {
  
  url <- gsub(" \\(excl. agencies\\)", "", path)
  url <- gsub(" \\(or equivalent\\)", "", url)
  url <- gsub(" \\(Non-GSG/GORS\\)", "-unbadged", url)
  url <- gsub(" \\(GSG/GORS\\)", "-badged", url)
  url <- gsub(" ", "-", url)
  url <- gsub(",", "", url)
  return(url)
  
}

