#'@title Load and preprocess data
#'
#'@description download API data, clean and dervice new variables.
#'
#'@return the exported data as a dataframe
#'
#'@export

preprocess <- function() {
  
  API_data <- ingest()
  
  if(API_data$status_code != 200) stop("Unsuccessful API request. Status code: ", API_data$status_code, "\n Process Killed.")
  
  carsurvey_data <- convert_raw(API_data) 
  carsurvey_data <- tidy_ingest(carsurvey_data) 
  carsurvey_data <- enforce_streaming(carsurvey_data) 
  carsurvey_data <- rename_cols(carsurvey_data) 
  carsurvey_data <- recode_grade(carsurvey_data) 
  carsurvey_data <- derive_rap_scores(carsurvey_data) 
  carsurvey_data <- derive_code_status(carsurvey_data)
  carsurvey_data <- merge_data_scientist(carsurvey_data)
  
  return(carsurvey_data)
}





#'@title Ingest smartsurvey data
#'
#'@description Download smartsurvey export via the API. Download the exported data from smartsurvey using the API. Use convert_raw() to convert the API response to a data.frame.
#'
#'@param survey the survey ID (character string/numeric). Defaults to "790800".
#'@param export the export ID (character string/numeric). Defaults to "1438876".
#'@param token the API token (character string). Loaded from environment variable by default.
#'@param secret the secret API token (character string). Loaded from environment variable by default.
#'@param check_hash a hash to check the downloaded data aginst or FALSE to skip the check (character string/logical). Defaults to final export hash.
#'
#'@return the exported data as a dataframe
#'
#'@export

ingest <- function(survey = "790800",
                   export = "1438876",
                   token = Sys.getenv("CARS_TOKEN"),
                   secret = Sys.getenv("CARS_SECRET"),
                   check_hash = "6c64deda0580f9cf9cc6a32a3445472b13179053") {
  
  # Check input types
  if (!is.character(survey) && !is.numeric(survey) | !is.character(export) && !is.numeric(export)) {
    stop("Unexpected input - survey and export should lbe character or numeric variables.")
  }
  
  if (!is.character(token) | !is.character(secret)) {
    stop("Unexpected input - token and secret should be character variables.")
  }
  
  if (!is.character(check_hash) && check_hash != FALSE) {
    stop("Unexpected input - check_hash should be a character variable or set to FALSE.")
  }
  
  if (length(survey) > 1 | length(token) > 1 | length(token) > 1 | length(secret) > 1 | length(check_hash) > 1) {
    stop("Unexpected input - one or more of the supplied arguments contain multiple elements.")
  }
  
  # API request
  url <- paste0("https://api.smartsurvey.io/v1/surveys/", survey, "/exports/", export, "/download") 
  
  query_string <- list(
    api_token = token,
    api_token_secret = secret
  )
  
  tryCatch(
    {
      r <- httr::GET(
        url, 
        query = query_string
      )    
    },
    error = function(e) {
      stop(paste("Error in API request: ", e))
    }
  )
  
  # Check request status code
  if (r$status_code != 200) {
    stop(paste0("Unsuccessful API request. Status code: ", r$status_code))
    return(r)
  }
  
  # Check hashes match
  if (is.character(check_hash)) {
    hash <- digest::digest(r$content, algo = "sha1", serialize = FALSE)
    if (hash != check_hash) {
      stop("hashed data does not match verification hash.")
    }
  }
  
  return(r)
}

#'@title Convert raw data to data.frame
#'
#'@description Convert raw smartsurvey data to data.frame . Extract contents (raw csv) from smartsurvey API request and convert to data.frame
#'
#'@param r api response object
#'
#'@return response content as a data.frame
#'
#'@export

convert_raw <- function(r) {
  
  if (class(r) != "response") {
    stop("Unexpected input - r is not a response object.")
  } else if (r$status_code != 200) {
    stop("Unsuccessful API request - no data.")
  }
  
  content <- rawToChar(r$content)
  
  data <- utils::read.table(
    text = content, 
    sep = ",", 
    header = TRUE, 
    fill = TRUE, 
    quote = "\"\"",
    na.strings = c("", ".", "NA", "-", "\"\"", "\".\"", "\"NA\"", "\"-\"")
  )
  
  return(data)
}

#'@title Tidy ingested data
#'
#'@description Tidy the ingested data from the smartsurvey API. Tidy column names and remove empty rows from the ingested data. 
#' The question numbers returned are only nested to one level, e.g. Q1.1, Q1.2, etc. However, the function will work on any number of nester levels or none. 
#' Use covert_raw() to convert the smartsurvey API response to a data.frame before using this function on the data.
#'
#'@param data the data returned by the ingest function (data.frame)
#'
#'@return the tidied data (data.frame)
#'
#'@export

tidy_ingest <- function(data) {
  
  # Check input
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data.frame")
  }
  
  # Rename headings
  # SmartSurvey data comes with question names spread over the first three rows and the full question names included
  # The code below relabels the columns with question numbers
  
  q_numbers <- sapply(
    colnames(data), function(colname) {
      if (substring(colname, 1, 1) == "Q") { # Return the question numbers from column names beginning with Q
        return(
          stringr::str_split(
            string = colname,
            pattern = "[.]",
            simplify = T
          )[1]
        )
      } else if (stringr::str_detect(colname, "X")) { # Return missing for empty column names
        return(NA)
      } else {
        return(colname)
      }
    }
  )
  
  q_numbers[1] <- "userID"
  
  # carry question numbers forward to replace missing question numbers
  q_numbers <- zoo::na.locf(q_numbers) 
  
  # Number duplicate column names
  q_freqs <- data.frame(table(q_numbers))
  q_freqs$q_numbers <-
    factor(q_freqs$q_numbers, levels = unique(q_numbers))
  q_freqs <- q_freqs[order(q_freqs$q_numbers),]
  
  # Append number to names where responses to a survey item are spread over multiple columnn
  # E.g. Returns Q1, Q1.1, Q1.2, Q1.3 if Q1 respobses appear in four different columns
  new_colnames = Map(
    function(name, freq) {
      if (freq == 1) {
        return(as.character(name))
      } else {
        return(c(paste0(rep(name, freq),
                        c(
                          "", rep(".", freq - 1)
                        ),
                        c("", c(
                          1:(freq - 1)
                        )))))
      }
    },
    q_freqs$q_numbers, 
    q_freqs$Freq
  )
  new_colnames <- purrr::flatten(new_colnames)
  
  colnames(data) <- new_colnames
  
  # Drop empty rows
  data <- data[!is.na(data$userID), ] 
  
  # Correct strings
  data <- data.frame(
    lapply(data, function(x) {
      gsub("Don@SQ@t Know", "Don't Know", x)
    })
  )
  
  data$IP.Address <- NULL
  
  return(data)
}

#'@title Rename columns
#'
#'@description Rename columns in CARS wave 2 dataset.
#'
#'@param data a data frame containing prepared CARS wave 2 data
#'
#'@return df with renamed columns
#'
#'@export

rename_cols <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  }
  
  tryCatch(
    {
      data <- dplyr::rename(data, 
                            dept = "Q1",
                            other_dept = "Q1.1",
                            grade = "Q2",
                            nonCS = "Q3",
                            GSG = "Q3.1",
                            GES = "Q3.2",
                            GSR = "Q3.3",
                            GORS = "Q3.4",
                            sci_eng = "Q3.5",
                            DDAT = "Q3.6",
                            GAD = "Q3.7",
                            finance = "Q3.8",
                            datasci_GSG = "Q3.9",
                            datasci_non = "Q3.10",
                            non_prof = "Q3.11",
                            other_prof = "Q3.12", 
                            location = "Q4",
                            produce_official_stats = "Q5",
                            
                            edu_level = "Q6",
                            maths = "Q7",
                            stats = "Q7.1",
                            systems = "Q7.2",
                            compsci = "Q7.3",
                            econ = "Q7.4",
                            psych = "Q7.5",
                            geog = "Q7.6",
                            socialsci = "Q7.7",
                            lifesci = "Q7.8",
                            physsci = "Q7.9",
                            engineer = "Q7.10",
                            business = "Q7.11",
                            health = "Q7.12",
                            law = "Q7.13",
                            history = "Q7.14",
                            lang_lit = "Q7.15",
                            other_subject = "Q7.16",
                            
                            code_freq = "Q8",
                            data_cleaning = "Q9",
                            data_analysis = "Q9.1",
                            data_vis = "Q9.2",
                            QA = "Q9.3",
                            data_transfer = "Q9.4",
                            other_ops = "Q9.5",
                            other_ops_comments = "Q9.6",
                            
                            knowledge_R = "Q10",
                            knowledge_SQL = "Q10.2",
                            knowledge_SAS = "Q10.4",
                            knowledge_VBA = "Q10.6",
                            knowledge_python = "Q10.8",
                            knowledge_SPSS = "Q10.10",
                            knowledge_stata = "Q10.12",
                            knowledge_JS = "Q10.14",
                            knowledge_java = "Q10.16",
                            knowledge_C = "Q10.18",
                            
                            available_R = "Q10.1",
                            available_SQL = "Q10.3",
                            available_SAS = "Q10.5",
                            available_VBA = "Q10.7",
                            available_python = "Q10.9",
                            available_SPSS = "Q10.11",
                            available_stata = "Q10.13",
                            available_JS = "Q10.15",
                            available_java = "Q10.17",
                            available_C = "Q10.19",
                            
                            other_tool = "Q10.20",
                            
                            code_experience = "Q11",
                            ability_change = "Q12",
                            learn_before = "Q13",
                            code_learn_where = "Q14",
                            
                            RAP_heard_of = "Q15",
                            RAP_champ = "Q16",
                            RAP_champ_known = "Q17",
                            RAP_understand = "Q18",
                            RAP_confident = "Q18.1",
                            RAP_important = "Q18.2",
                            RAP_supported = "Q18.3",
                            RAP_resources = "Q18.4",
                            RAP_using = "Q18.5",
                            comments_RAP = "Q18.6",
                            
                            gp_open_source = "Q19",
                            gp_dir_structure = "Q19.1",
                            gp_guidelines = "Q19.2",
                            gp_version_control = "Q19.3",
                            gp_code_review = "Q19.4",
                            gp_function = "Q19.5",
                            gp_packages = "Q19.6",
                            gp_unit_test = "Q19.7",
                            gp_auto_QA = "Q19.8",
                            gp_team_open_source = "Q19.9",
                            
                            doc_AQA_log = "Q20",
                            doc_assumption_reg = "Q20.1",
                            doc_func = "Q20.2",
                            doc_comments = "Q20.3",
                            doc_flow = "Q20.4",
                            doc_readme = "Q20.5",
                            doc_desk = "Q20.6",
                            doc_other = "Q20.7",
                            
                            use_cont_integration = "Q21",
                            use_dependency_management = "Q22",
                            use_reprod_workflow = "Q23",
                            comments_coding_practices ="Q24",
                            use_github = "Q25",
                            use_gitlab = "Q25.1",
                            use_bitbucket = "Q25.2",
                            use_AWS = "Q25.3",
                            use_googlecloud = "Q25.4",
                            use_other = "Q25.5",
                            
                            comments_coding_support = "Q26",
                            comments_survey = "Q27",
                            comments_other = "Q28")
    }, 
    error = function(e) {
      stop("Incorrect column names - use cleaned smartsurvey API data")
    }
  )
  
  return(data)
  
}

#'@title Derive RAP scores
#'
#'@description Calculate RAP scores. Derive RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 2 data
#'
#'@return df containing the additional RAP score columns
#'
#'@export

derive_rap_scores <- function(data) {
  
  # Check input
  expected_cols <- c("gp_code_review", 
                     "gp_version_control", 
                     "gp_open_source" ,
                     "gp_team_open_source", 
                     "doc_readme" ,
                     "doc_comments", 
                     "gp_function" ,
                     "gp_unit_test",
                     "doc_func" ,
                     "gp_packages", 
                     "gp_guidelines",
                     "use_cont_integration",
                     "use_dependency_management")
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_cols %in% colnames(data))) {
    missing <- paste(expected_cols[!(expected_cols %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }
  
  #Derive RAP scores
  high_vals <- c("Regularly", "All the time")
  
  # Basic components
  data$peer_review_score <- ifelse(data$gp_code_review %in% high_vals, 1, 0)
  data$version_control_score <- ifelse(data$gp_version_control %in% high_vals, 1, 0)
  data$use_open_source_score <- ifelse(data$gp_open_source %in% high_vals, 1, 0)
  data$open_code_score <- ifelse(data$gp_team_open_source %in% high_vals, 1, 0)
  data$doc_score <- ifelse(data$doc_readme %in% high_vals & data$doc_comments %in% high_vals, 1, 0)
  
  data$basic_rap_score <- rowSums(data[,c("peer_review_score", 
                                          "version_control_score",
                                          "use_open_source_score",
                                          "open_code_score",
                                          "doc_score")])
  
  # Advanced components
  data$function_score <- ifelse(data$gp_function %in% high_vals, 1, 0)
  data$test_score <- ifelse(data$gp_unit_test %in% high_vals, 1, 0)
  data$function_doc_score <- ifelse(data$doc_func %in% high_vals, 1, 0)
  data$package_score <- ifelse(data$gp_packages %in% high_vals, 1, 0)
  data$code_style_score <- ifelse(data$gp_guidelines %in% high_vals, 1, 0)
  data$cont_integreation_score <- ifelse(data$use_cont_integration == "Yes", 1, 0)
  data$dep_management_score <- ifelse(data$use_dependency_management == "Yes", 1, 0)
  
  data$advanced_rap_score <- rowSums(data[,c("function_score", 
                                             "test_score", 
                                             "function_doc_score", 
                                             "package_score", 
                                             "code_style_score", 
                                             "cont_integreation_score", 
                                             "dep_management_score")])
  
  return(data)
}

#'@title Derive code status
#'
#'@description Derive coding tool knowledge but no access/access but no knowledge status..
#'
#'@param data a data frame containing cleaned CARS wave 2 data
#'
#'@return df containing the coding tool status columns
#'
#'@export

derive_code_status <- function(data) {
  # Check input
  expected_cols <- c(
    "knowledge_R",
    "knowledge_SQL",
    "knowledge_SAS",
    "knowledge_VBA",
    "knowledge_python",
    "knowledge_SPSS",
    "knowledge_stata",
    "knowledge_JS",
    "knowledge_java",
    "knowledge_C",
    "available_R",
    "available_SQL",
    "available_SAS",
    "available_VBA",
    "available_python",
    "available_SPSS",
    "available_stata",
    "available_JS",
    "available_java",
    "available_C"
  )
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_cols %in% colnames(data))) {
    missing <-
      paste(expected_cols[!(expected_cols %in% colnames(data))], collapse = "\n")
    stop(paste0("Unexpected input - missing column names: ", missing))
  }
  
  lang_cols <- colnames(data)[grepl("available_", colnames(data))]
  langs <- as.vector(stringr::str_split(lang_cols, "_", simplify = T)[,2])
  
  new_cols <- lapply(langs, function(lang) {
    k_column <- as.vector(data[paste0("knowledge_", lang)] == "Yes")
    a_column <- as.vector(data[paste0("available_", lang)] == "Yes")
    new_col <- paste0(a_column, k_column)
    new_col[new_col == "TRUETRUE"] <- "Access and knowledge"
    new_col[new_col == "TRUEFALSE"] <- "Access only"
    new_col[new_col == "FALSETRUE"] <- "Knowledge only"
    new_col[new_col == "FALSEFALSE"] <- "No access or knowledge"
    return(new_col)
  })
  
  new_colnames <- paste0("status_", langs)
  data[new_colnames] <- new_cols
  
  return(data)
}


#'@title Recode grade
#'
#'@description Recode non-set responses to grade question
#'
#'@param data a data frame containing cleaned CARS wave 2 data
#'@param grade_col the name of the column containing the grade (set to "grade" by default)
#'@param dep_col the name of the column containing the department (set to "dept" by default)
#'
#'@return the original data with the reocded grade column
#'
#'@export


recode_grade <- function(data, grade_col = "grade", dep_col = "dept") {
  
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  } else if (!grade_col %in% colnames(data)) {
    stop("Unexpected input - data does not contain the grade column")
  } else if (!dep_col %in% colnames(data)) {
    stop("Unexpected input - data does not contain the department column")
  }
  
  final_grades <- c(
    "Administrative Officer (or equivalent)",
    "Executive Officer (or equivalent)",
    "Higher Executive Officer (or equivalent)",                             
    "Senior Executive Officer (or equivalent)",
    "Grade 7 (or equivalent)",                 
    "Grade 6 (or equivalent)",
    "Fast Stream",
    "Other - NHS", 
    "Other - DSTL"
  )
  
  # Merge NHS into single category
  data[[grade_col]][grepl("nhs", data[[dep_col]], ignore.case=TRUE)] <- "Other - NHS"
  data[[grade_col]][grepl("nhs", data[[grade_col]], ignore.case=TRUE)] <- "Other - NHS"
  
  # Recode DSTL grades (based on equivalent grades on Civil Service Jobs)
  dstl_mask <- data[[dep_col]] == "Defence Science and Technology Laboratory"
  data[[grade_col]][dstl_mask & grepl("level 1|l1|grade 1|level 2|l2|grade 2|level 3|l3|grade 3", data[[grade_col]], ignore.case=TRUE)] <- "Administrative Officer (or equivalent)"
  data[[grade_col]][dstl_mask & grepl("level 4|l4|grade 4", data[[grade_col]], ignore.case=TRUE)] <- "Higher Executive Officer (or equivalent)"
  data[[grade_col]][dstl_mask & grepl("level 5|l5|grade 5", data[[grade_col]], ignore.case=TRUE)] <- "Senior Executive Officer (or equivalent)"
  data[[grade_col]][dstl_mask & grepl("level 6|l6|level 7|l7", data[[grade_col]], ignore.case=TRUE)] <- "Grade 7 (or equivalent)"
  
  data[[grade_col]][!data[[grade_col]] %in% final_grades & dstl_mask] <- "Other - DSTL"
  
  data[[grade_col]][!data[[grade_col]] %in% final_grades] <- "Other"
  
  return(data)

}


#'@title Merge data science professions
#'
#'@description Merge data science columns into one column named datasci
#'
#'@param data a data frame containing cleaned CARS wave 2 data
#'
#'@return the original data with the merged data science column
#'
#'@export

merge_data_scientist <- function(data){
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  } else if (!"datasci_GSG" %in% colnames(data)) {
    stop("Unexpected input - data does not contain the data science GSG badged column")
  } else if (!"datasci_non" %in% colnames(data)) {
    stop("Unexpected input - data does not contain the data science non badged column")
  }
  
  datasci <- ifelse((data$datasci_GSG =="Yes" | data$datasci_non == "Yes") |
                         (data$datasci_GSG =="Yes" & data$datasci_non == "Yes"),
                         "Yes","No")
  data <- tibble::add_column(data, datasci, .after = "datasci_non")
  data[c("datasci_non","datasci_GSG")] <- NULL
  return(data)
}

