#'@title build html table forlikert outputs (kable)
#'
#'@description build output table for likert question data
#'
#'@param table Frequency table for likert quesitons (data frame). 4+ columns - question names in column 1 with answer options in subsequent columns. Frequencies should proportions, between 0 and 1. 
#'
#'@return html table
#'
#'@export

output_likert_table <- function(table) {
  
  table[2:length(table)] <- table[2:length(table)] * 100
  table[2:length(table)] <- round(table[2:length(table)], 1)
  rownames(table) <- NULL
  
  html_table <- knitr::kable(table, format = "html")
  html_table <- kableExtra::add_header_above(html_table, c(" " = 1, "Percent" = ncol(table)-1))
  return(html_table)
}
