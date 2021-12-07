
# README
# Functions defined here are used in generate_tables()

# All functions are to take data as the first argument
# Any objects needed, such as a list of languages are to be defined in generate_tables() and passed into the function.

# The only output of the function in the generated table, no additional objects to be returned or shared with other functions.

###########################

#' @title Coding usage
#' 
#' @description Function which calculates the frequencies of time spent coding
#'
#' @param data This is generated using the carsurvey2::data functions.
#'
#' @return data.frame
#' 
#' @export 
#'

calc_freqs_coding <- function(data) {
  
  data$code_freq <- factor(data$code_freq, levels = c("Never",
                                                      "Rarely",
                                                      "Sometimes",
                                                      "Regularly",
                                                      "All the time"))
  freq_table <- data.frame(table(data$code_freq))
  
  colnames(freq_table) <- c("Coding frequency", "Count")
  
  return(freq_table)
  
}

#' @title Knowledge of coding langauges
#' 
#' @description Function which creates a table of the frequencies for knowledge of coding langauges
#'
#' @param data This is generated using the carsurvey2::data functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' 
#' @export

calc_freqs_knowledge <- function(data, langs) {
  
  knowledge <- data[grepl("knowledge_", colnames(data))]
  knowledge <- carsurvey2::calc_multi_col_freqs(cols = knowledge, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(knowledge) <- c("Programming language", "Yes", "Don't know", "No")
  knowledge[[1]] <- dplyr::recode(stringr::str_split(knowledge[[1]], "_", simplify = TRUE)[,2 ],
                                  !!!langs) # Rename questions
  knowledge <- knowledge[order(knowledge[1]),]
  knowledge[1] <- factor(knowledge[[1]], levels = knowledge[[1]])
  return(knowledge)
  
}



#' @title Access to coding langauges
#' 
#' @description Function which creates a table of the frequencies for access to coding langauges
#'
#' @param data This is generated using the carsurvey2::data functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' @export

calc_freqs_access_lang <- function(data, langs) {
  
  access <- data[grepl("available_", colnames(data))]
  access <- carsurvey2::calc_multi_col_freqs(cols = access, factor_levels = c("Yes", "Don't Know", "No"))
  colnames(access) <- c("Programming language", "Yes", "Don't know", "No")
  access[[1]] <- dplyr::recode(stringr::str_split(access[[1]], "_", simplify = TRUE)[,2],
                               !!!langs) # Rename questions
  access <- access[order(access[1]),]
  access[1] <- factor(access[[1]], levels = access[[1]])
  
  return(access)
  
}

#' @title Availability and knowledge of coding langauges
#' 
#' @description Function which creates a table of the frequencies for access and knowledge of coding langauges
#'
#' @param data This is generated using the carsurvey2::data functions.
#' 
#' @param langs Formatted list of strings. langs object is available in carsurvey2::generate_tables()
#'
#' @return data.frame
#' 
#' @export

calc_freqs_coding_tools <- function(data, langs) {
  
  code_tool_status <- data[grepl("status_", colnames(data))]
  code_tool_status <- carsurvey2::calc_multi_col_freqs(cols = code_tool_status, factor_levels = c("Access only", "Access and knowledge", "Knowledge only"))
  colnames(code_tool_status) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only") 
  code_tool_status[[1]] <- dplyr::recode(stringr::str_split(code_tool_status[[1]], "_", simplify = TRUE)[,2],
                                         !!!langs) # Rename questions
  code_tool_status <- code_tool_status[order(code_tool_status[1]),]
  code_tool_status[1] <- factor(code_tool_status[[1]], levels = code_tool_status[[1]])
  return(code_tool_status)
}


#' @title Knowledge of RAP
#' 
#' @description Create a frequency table of knowledge of RAP
#'
#' @param data This is generated using the carsurvey2::data functions.
#'
#' @return data.frame
#' @export


calc_freqs_knowledge_of_rap <- function(data) {
  
  data$RAP_champ_known[data$RAP_heard_of == "No"] <- "Have not heard of RAP"
  
  data$RAP_champ_known <- factor(data$RAP_champ_known, levels = c(
    "Have not heard of RAP",                                     
    "I don't know what a RAP champion is",                          
    "I know what a RAP champion is but don't know who the RAP champion in my department is",
    "I know what a RAP champion is and there is no RAP champion in my department",
    "I know who the RAP champion in my department is"
  ))
  
  rap_knowledge <- data.frame(table(data$RAP_champ_known))
  
  colnames(rap_knowledge) <- c("RAP champion knowledge", "Count")
  rap_knowledge[1] <- c("Have not heard of RAP",
                        "Heard of RAP, have not heard of RAP champions",
                        "Heard of RAP, does not know department champion",
                        "Heard of RAP champions, no champion in department",
                        "Knows department RAP champion")
  
  rap_knowledge_chart <- rap_knowledge
  rap_knowledge_chart[[1]] <- factor(rap_knowledge_chart[[1]], levels = rap_knowledge_chart[[1]])
 
  return(rap_knowledge_chart)
}



#' @title Opinion of RAP
#' 
#' @description Create frequency table of opinions of RAP
#'
#' @param data This is generated using the carsurvey2::data functions.
#'
#' @return data.frame
#' @export

calc_freqs_opinion_of_rap <- function(data) {
  
  know_rap_data <- data[data$RAP_heard_of == "Yes", ]
  know_rap_data <- dplyr::select(know_rap_data, "RAP_understand":"RAP_using")
  know_rap_levels <- c("Strongly Disagree",
                       "Disagree",
                       "Neutral",
                       "Agree",
                       "Strongly Agree")
  rap_opinions <- carsurvey2::calc_multi_col_freqs(know_rap_data, know_rap_levels, calc_props=TRUE)
  new_colnames <- c(RAP_understand = "I understand what the key components of the RAP methodology are",
                    RAP_confident = "I feel confident implementing RAP in my work",
                    RAP_important = "I think it is important to implement RAP in my work",
                    RAP_supported = "I feel supported to implement RAP in my work",
                    RAP_resources = "I know where to find resources to help me implement RAP",
                    RAP_using = "I and/or my team are currently implementing RAP")
  
  rap_opinions[[1]] <- dplyr::recode(rap_opinions[[1]], !!!new_colnames) 
  colnames(rap_opinions) <- c("Question",
                              "Strongly disagree",
                              "Disagree",
                              "Neutral",
                              "Agree",
                              "Strongly agree")
  rap_opinions_chart <- rap_opinions
 
  return(rap_opinions_chart)
   
}


#' @title RAP score components
#' 
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data This is generated using the carsurvey2::data functions.
#'
#' @return data.frame
#' @export

calc_freqs_rap_score_components <- function(data) {
  
  rap_score <- data[grepl("_score", colnames(data))]
  
  components <- rap_score[!colnames(rap_score) %in% c("basic_rap_score", "advanced_rap_score")]
  components[is.na(components)] <- 0
  components <- data.frame(colSums(components))
  components <- data.frame(Component = rownames(components), Count = unname(components[1]))
  
  basic_comps <-c(
    "peer_review_score",
    "version_control_score",
    "use_open_source_score",
    "doc_score",
    "open_code_score"
  )
  
  components$Type <- ifelse(components$Component %in% basic_comps, "Basic", "Advanced")
  components$Component <- dplyr::recode(components$Component, 
                                        "peer_review_score" = "Peer review",
                                        "version_control_score" = "Version control",
                                        "use_open_source_score" = "Use open source software",
                                        "open_code_score" = "Team open source code",
                                        "doc_score" = "Documentation",
                                        "function_score" = "Functions",
                                        "test_score" = "Unit testing",
                                        "function_doc_score" = "Function documentation",
                                        "package_score" = "Code packages",
                                        "code_style_score" = "Follow code style guidelines",
                                        "cont_integreation_score" = "Continuous integration",
                                        "dep_management_score" = "Dependency management")
  
  components <- components[with(components, order(-rank(Type),Component)), ]  
  components$Component <- factor(components$Component, levels = components$Component)
  components <- components[c(1, 3, 2)]

  rownames(components) <- NULL
  
  return(components)
}


#' @title RAP score for basic components
#' 
#' @description Create frequency table of RAP score basic components
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#'
#' @return data.frame
#' @export

calc_freqs_rap_basic <- function(data) {
  
  basic_freqs <- data.frame(table(data$basic_rap_score))
  colnames(basic_freqs) <- c("Basic RAP score", "Count")
  
  return(basic_freqs)
}

#' @title RAP score for advanced components
#' 
#' @description Create frequency of RAP score for advanced components
#'
#' @param data This is generated using the carsurvey2::data functions.
#'
#' @return data.frame
#' @export

calc_freqs_rap_advanced <- function(data) {
  
  advanced_freqs <- data.frame(table(data$advanced_rap_score))
  colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
  
  return(advanced_freqs)
  
}

#' @title Types of coding practices used
#' 
#' @description Create frequency table of the types of coding practices used
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#'
#' @return data.frame
#' @export

calc_freqs_practices_usage <- function(data, code_prac_levels) {
  
  code_prac_chart <- carsurvey2::calc_freqs_coding_practices(data, code_prac_levels)
                                    
  return(code_prac_chart)
}

#' @title Use of documentation
#'
#' @description Create frequency table of the types of documentation used
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param code_prac_levels A vector of strings, length 6
#' 
#' @return data.frame
#' @export

calc_freqs_documenation_usage <- function(data, code_prac_levels) {
  
  code_prac = carsurvey2::calc_freqs_coding_practices(data, code_prac_levels)
  
  doc_data <- dplyr::select(data, "doc_AQA_log":"doc_desk")
  doc_data <- doc_data[data$code_freq != "Never", ]

  doc <- carsurvey2::calc_multi_col_freqs(doc_data, code_prac_levels, calc_props = TRUE)

  colnames(doc)[1] <- "Question"
  colnames(doc)[c(2:length(doc))] <- code_prac_levels
  
  doc_questions <- c(doc_AQA_log = "Analytical Quality Assurance (AQA) logs",
                     doc_assumption_reg = "Data or assumptions registers",
                     doc_func = "Documentation for each function or class",
                     doc_comments = "Code comments",
                     doc_flow = "Flow charts",
                     doc_readme = "README files",
                     doc_desk  = "Desk notes")
  
  doc[[1]] <- dplyr::recode(doc[[1]], !!!doc_questions)
  doc <- dplyr::arrange(doc, "Question")
  
  return(doc)
}


#' @title Types of coding practices used
#' 
#' @description Generate more than 1 table = calc_freqs_coding_practice_usage, calc_freqs_documenation_usage
#'
#' @param data This is generated using the data functions.
#' @param code_prac_levels A vector of strings, length 6
#'
#' @return data.frame
#' @export

calc_freqs_coding_practices <- function(data, code_prac_levels) {
  
  code_prac_data <- data[grepl("gp_", colnames(data))]
  code_prac_data <- code_prac_data[data$code_freq != "Never", ]
  
  code_prac <- carsurvey2::calc_multi_col_freqs(code_prac_data, code_prac_levels, calc_props = TRUE)
  
  colnames(code_prac)[1] <- "Question"
  colnames(code_prac)[c(2:length(code_prac))] <- code_prac_levels
  
  code_prac_questions <- c(
    gp_open_source = "I use open source software when programming",
    gp_dir_structure = "I follow a standard directory structure when programming",
    gp_guidelines = "I follow coding guidelines or style guides when programming",
    gp_version_control = "I use a source code version control system e.g. Git",
    gp_code_review = "Code my team writes is reviewed by a colleague",
    gp_function = "I write repetitive elements in my code as functions",
    gp_packages = "I collect my code and supporting material into packages",
    gp_unit_test = "I unit test my code",
    gp_auto_QA = "I write code to automatically quality assure data",
    gp_team_open_source = "My team open sources its code"
  )
  
  code_prac[[1]] <- dplyr::recode(code_prac[[1]], !!!code_prac_questions) 
  
  return(code_prac)
}


#' @title Operations and how they are preformed
#'
#' @description Create frequency table of operations and how they are preformed 
#'
#' @param data This is generated using the carsurvey2:: functions
#'
#' @return data.frame
#'
#' @export

calc_freq_operations <- function(data){
  
  operations_data <- dplyr::select(data,"data_cleaning":"data_transfer")
  levels <- c("I do this without coding",
              "I do some or all of this by coding")
  
  operations <- carsurvey2::calc_multi_col_freqs(operations_data, levels)
  
  code_prac_questions <- c(
    data_analysis = "Data Analysis",
    data_cleaning = "Data Cleaning",
    data_transfer = "Data Transfer / Migration",
    data_vis = "Data Visualisation",
    QA = "Quality Assurance")
    
  
  operations[[1]] <- dplyr::recode(operations[[1]], !!!code_prac_questions) 
  
  colnames(operations) <- c("Data operation", "I do this without coding", "I do some or all of this by coding")
  operations <- operations[order(operations[1]),]
  
  return(operations)
}

#' @title Coding experience outside of work frequency table
#'
#' @description Create frequency table of if any coding experience outside current role
#'
#' @param data This is generated using the carsurvey2:: functions.
#'
#' @return data.frame
#' 
#' @export 
#'

calc_freqs_outside_work <- function(data) {
  data$code_experience <- factor(data$code_experience, levels = c("Yes",
                                                                  "No"))
  
  frequency_table <- data.frame(table(data$code_experience))
  
  colnames(frequency_table) <- c("code experience outside of work", "count")
  
  return(frequency_table)
}


#' @title Has coding ability changed frequency table
#'
#' @description Create frequency table of coding ability changes in current role
#'
#' @param data This is generated using the carsurvey2:: functions.
#'
#' @return data.frame
#' 
#' @export

calc_freq_ability <- function(data) {
  
  data$ability_change <- factor(data$ability_change, levels = c("Significantly worse",
                                                                "Slightly worse",
                                                                "No change",
                                                                "Slightly better",
                                                                "Significantly better"))
  frequency_table <- data.frame(table(data$ability_change))
  
  colnames(frequency_table) <- c("Coding ability changes", "Count")
  
  return(frequency_table)
  
}


#' @title First learnt code frequency table
#'
#' @description Create frequency table about where first coding was learnt
#'
#' @param data This is generated using the carsurvey2:: functions.
#'
#' @return data.frame
#'
#' @export

calc_freq_learn_code <- function(data) {
  
  levels <- c("In education",
              "In private sector employment",
              "In public sector employment",
              "Self-taught",
              "Other")
  
  # Recode other responses
  data$code_learn_where[!is.na(data$code_learn_where) & !data$code_learn_where %in% levels ] <- "Other"
  
  data$code_learn_where <- factor(data$code_learn_where, levels = levels)
  
  frequency_table <- data.frame(table(data$code_learn_where))
  
  frequency_table[[1]] <- factor(frequency_table[[1]], c("In current role", levels))
  
  frequency_table <- rbind(
    c(
      "In current role", sum(
        (is.na(data$learn_before) | data$learn_before == "No") & data$code_freq != "Never")
      ), 
    frequency_table)
  
  frequency_table[[2]] <- as.numeric(frequency_table[[2]])
  
  colnames(frequency_table) <- c("First coding experience", "Count")
  
  return(frequency_table)
}


#' @title Knowledge of reproducible workflow packages frequency table
#'
#' @description Create frequency table of knowledge of reproducible workflow packages
#'
#' @param data This is generated using the carsurvey2:: functions.
#'
#' @return data.frame
#'
#' @export

calc_freq_reproducible_workflow <- function(data){
  
  data$use_reprod_workflow <- factor(data$use_reprod_workflow, levels = c("Yes",
                                                                 "No",
                                                                 "I don't know what reproducible workflows are"))
  frequency_table <- data.frame(table(data$use_reprod_workflow))
  
  colnames(frequency_table) <- c("Use reproducible workflow packages", "Count")
  frequency_table[[1]] <- c("Yes","No","Don't know what they are")
  
  code_prac_questions <- c(
    Yes = "Yes",
    No = "No",
    "I don't know what reproducible workflows are" = "Don't know what they are"
    )
  
  frequency_table[[1]] <- dplyr::recode(frequency_table[[1]], !!!code_prac_questions) 
  
  frequency_table[[1]] <- factor(frequency_table[[1]], levels = frequency_table[[1]])
  
  return(frequency_table)
}


#' @title Version control platforms frequency table
#'
#' @description Create frequency table of use of version control software
#'
#' @param data This is generated using the carsurvey2:: functions.
#'
#' @return data.frame
#'
#' @export

calc_freq_version_control <- function(data){
  
  version_control_platforms <- dplyr::select(data, "use_github":"use_googlecloud")
  levels = c("Yes", "No") 
  version_platform_freqs <- carsurvey2::calc_multi_col_freqs(version_control_platforms, levels)

  code_prac_questions <- c(
    use_github = "GitHub",
    use_gitlab = "GitLab",
    use_bitbucket = "BitBucket",
    use_AWS = "AWS CodeCommit",
    use_googlecloud = "Cloud Source Repository (Google Cloud)")
  
  version_platform_freqs[[1]] <- dplyr::recode(version_platform_freqs[[1]], !!!code_prac_questions) 
  
  version_platform_freqs <- version_platform_freqs[c(1,2)]
  
  colnames(version_platform_freqs) <- c("Version control platform","Yes")
  
  version_platform_freqs <- dplyr::arrange(version_platform_freqs, "Version control platform")
  
  return(version_platform_freqs)
}



