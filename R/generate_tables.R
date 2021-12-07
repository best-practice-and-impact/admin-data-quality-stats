#' @title Generate tables
#' 
#' @description Creates a series of tables 
#' 
#' @details This is a high level function that is used to create a series of tables. The tables are defined in this package as a functions, all prefioxxed with table_.
#' This function owns some objects used in the table formatting functions e.g langs, these are passed into the functions. Each table created is added to a named list.
#' To add additional tables create a function in the table.R file, prefix with table_, add to this function and assign to the object tables.
#'
#' @param smart_survey_data This is generated using the carsurvey2::smart_survey_data_ functions.
#'
#' @return A named list
#' 
#' @export

generate_tables <- function(smart_survey_data) {
  
  # List of tables to be returned by the function
  # Add all generated tables to this list
  tables = list()
  
  ################################################
  # Objects to be used in  the table_ functions.
  
  # Programming tools
  langs <- c(
    C = "C++ / C#",
    java = "Java / Scala",	
    JS = "Javascript / Typescript",
    python = "Python",
    R = "R",
    SAS = "SAS",	
    SPSS = "SPSS",	
    SQL = "SQL",	
    stata = "Stata",	
    VBA = "VBA"
  )
  
  code_prac_levels = c("I don't understand this question",
                       "Never",
                       "Rarely",
                       "Sometimes",
                       "Regularly",
                       "All the time")
  
  # End of objects
  
  # Create tables
  
  tables$freq_table <- calc_freqs_coding(smart_survey_data)
 
  tables$knowledge <- calc_freqs_knowledge(smart_survey_data, langs)
  
  tables$access <- calc_freqs_access_lang(smart_survey_data, langs)
  
  tables$code_tool_status <- calc_freqs_coding_tools(smart_survey_data, langs)
  
  tables$rap_knowledge_chart <- calc_freqs_knowledge_of_rap(smart_survey_data)
  
  tables$rap_opinions_chart <- calc_freqs_opinion_of_rap(smart_survey_data)

  tables$components <- calc_freqs_rap_score_components(smart_survey_data)
  
  tables$basic_freqs <- calc_freqs_rap_basic(smart_survey_data)
  
  tables$advanced_freqs <- calc_freqs_rap_advanced(smart_survey_data)
  
  tables$code_prac_chart <- calc_freqs_practices_usage(smart_survey_data, code_prac_levels)
  
  tables$doc <- calc_freqs_documenation_usage(smart_survey_data, code_prac_levels)
  
  tables$freq_ops <- calc_freq_operations(smart_survey_data)
  
  tables$outside_work <- calc_freqs_outside_work(smart_survey_data)
  
  tables$freq_abil <- calc_freq_ability(smart_survey_data)
    
  tables$first_learnt <- calc_freq_learn_code(smart_survey_data)
  
  tables$rep_workflow <- calc_freq_reproducible_workflow(smart_survey_data)
  
  tables$vers_cont <- calc_freq_version_control(smart_survey_data)
  
  # Error handling
  # Check tables 

  for (table in names(tables)) {
    
    # Check that only smart_survey_dataframes have be stored
    if(!class(tables[[table]]) == "data.frame") stop("Tables contains a non data.frame. /n Check the contents of the tables list ")
    
    # Check that tables contains rows
    if(nrow(tables[[table]]) == 0) stop(table, " is empty")
    
  } 
  
  return(tables)
  
}







