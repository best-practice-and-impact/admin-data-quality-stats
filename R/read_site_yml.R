#'@title Read RMarkdown site yaml
#'
#'@description Read the rmarkdown site yaml and return list of navbar arguments 
#'
#'@param filename the yaml file name (including path). Optional if text argument is present.
#'@param text character string - the contents of the yaml file. Optional if filename is present.
#'
#'@return list of arguments 
#'
#'@export

read_site_yml <- function(filename, text) {
  
  # Read yaml
  if (missing(filename) & missing(text)) {
    stop("No filename or text entered")
  } else if (!missing(filename)) {
    yml <- yaml::read_yaml(filename)
  } else {
    yml <- yaml::read_yaml(text = text)
  }
  
  # Find relevant fields
  if (is.null(yml$navbar$title) | is.null(yml$navbar$left)) {
    stop("Required fields missing in yaml file.")
  } else {
    navbar_info <- list(
      title = yml$navbar$title,
      pages = yml$navbar$left
    )
    
    return(navbar_info)
  }
  
}
