# High Level functions that are called to build the site. Prefixed render_

#' @title Render main site
#'
#' @description Processes the Smart survey data to generate a series of tables. 
#' Then removes the old site and uses the generated tables to render a new site.
#' Uses the markdown files located in the internal var markdown_file_path.
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param markdown_file_path the path containing the rmarkdown site documents
#'
#' @export 


render_main_site <- function(data, markdown_file_path = "rmarkdown/main") {

  knitr::opts_chunk$set(warning = FALSE)
  
  samples <- list(
    all = nrow(data),
    coders = sum(data$code_freq != "Never"),
    heard_of_rap = sum(data$RAP_heard_of == "Yes"),
    code_outside_current_role = sum(data$code_experience == "Yes"),
    any_code_experience = sum(data$code_experience == "Yes" | data$code_freq != "Never")
  )
  
  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path, preview = FALSE)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}

#' @title Render navigation bar
#'
#' @description Creates the site navbar.
#' 
#' @param yml_path Path to the rmarkdown site yml config
#' 
#' @return navbar_page html code for navbar
#' 
#' @export

render_navbar <- function(yml_path = "rmarkdown/main/_site.yml") {
  
  # Create navigation bar
  navbar_info <- read_site_yml(yml_path)
  navbar_page <- build_navbar(navbar_info)
  
  return(navbar_page)
}

#' @title Save navigation bar
#'
#' @description Saves the site navbar.
#' 
#' @param code html code (string)
#' @param path path for saving the navigation bar (excluding file name)
#' 
#' @export

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
}

#' @title Render filtered pages
#' 
#' @description Creates pages by filter (e.g. by department/profession/grade).
#' 
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param filter_variable The variable that the data is filtered on. This is given as a string such as "dept". 
#' The data is filtered by getting all values in the filter_variable greater than 20 and subsetting the data.
#' @param page_title This is ONLY PART of the title. page_title , " profile: ", filter -- 
#' where filter is one element of the list as described above in filter_variable (values over 20) and the page title is this argument.
#' @param output_folder Folder the site is built and saved to
#' @param template_path The path to the template that gets render for each department
#' @export


render_filtered_pages <- function(data,
                               filter_variable,
                               page_title = "",
                               output_folder = "../../docs",
                               template_path = "rmarkdown/summary_template/template.rmd") {
  
  if(!sum(colnames(data) == filter_variable) == 1) stop("filter column: ", filter_variable,
                                                                     " doesn't exist in the data provided. \nCheck that filter is equal to a valid column name")
  
  # get grade with sample > 20
  filter_table <- data.frame(
    table(data[filter_variable])
  )
  filter_over_20 <- filter_table[filter_table[2] >= 20, ]
  filter_list = as.character(filter_over_20$Var1)
  
  for (filter in filter_list) {
    
    file_path <- format_filter_path(filter)
    message("Writing page for ", file_path)
    
    # filter data to just the department
    filtered_data <- data[data[filter_variable] == filter, ]
    if(!nrow(filtered_data) == filter_over_20$Freq[filter_over_20$Var1 == filter]) stop("Filtered data row number is not equal to number of : ", file_path)
    
    title <- paste0(page_title , " profile: ", filter)
    
    filtered_tables <- generate_tables(filtered_data)
    
    samples <- list(
      all = nrow(filtered_data),
      coders = sum(filtered_data$code_freq != "Never"),
      heard_of_rap = sum(filtered_data$RAP_heard_of == "Yes"),
      code_outside_current_role = sum(filtered_data$code_experience == "Yes"),
      any_code_experience = sum(filtered_data$code_experience == "Yes" | filtered_data$code_freq != "Never")
    )
    
    knitr::opts_chunk$set(warning = FALSE)
    
    rmarkdown::render(template_path, 
                      output_file = paste0(output_folder, "/", file_path),
                      quiet = TRUE,
                      params = list(
                        title = title, 
                        tables = filtered_tables,
                        samples = samples
                      ))
  } 
}

#' @title Render profession pages
#' 
#' @description Creates pages by multi-column filter (profession)
#' 
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param page_title This is ONLY PART of the title. The title is generated from "DRAFT: ", page_title , " profile: ", filter -- 
#' where filter is one element of the list as described above in filter_variable (values over 20) and the page title is this argument.
#' @param output_folder Folder the site is built and saved to
#' @param template_path The path to the template that gets render for each department
#' @export


render_prof_pages <- function(data,
                              page_title = "",
                              output_folder = "../../docs",
                              template_path = "rmarkdown/summary_template/template.rmd") {
  
  if(!exists("data")) stop("Dataframe called data not available. This should be in the function enviroment of render_main_site. Check that this is available in this enviroment.")
  
  profs <- dplyr::select(data, "nonCS":"non_prof")
  prof_freqs <- calc_multi_col_freqs(profs, c("Yes", "No"))
  prof_freqs <- prof_freqs[c(1,2)]
  
  prof_freqs <- prof_freqs[prof_freqs[2] >= 20, ]
  prof_freqs[1] <- as.character(prof_freqs[[1]])
  colnames(prof_freqs) <- c("Profession", "Sample size")
  
  recode_vals <- c(
    nonCS = "Non Civil Service",
    GSG = "Government Statistician Group",
    GES = "Government Economic Service",
    GSR = "Government Social Research",
    GORS = "Government Operational Research Service",
    sci_eng = "Government Science and Engineering",
    DDAT = "Digital, Data and Technology Profession",
    datasci = "Government Data Scientist",
    non_prof = "Civil Service, no profession membership"
  )
  
  filter_list <- prof_freqs$Profession
  filter_names <- dplyr::recode(prof_freqs$Profession, !!!recode_vals)
  
  for (i in c(1:length(filter_list))) {
    
    filter_col <- filter_list[i]
    name <- filter_names[i]
    
    file_path <- format_filter_path(name)
    message("Writing page for ", file_path)
    
    filtered_data <- data[data[filter_col] == "Yes", ]
    
    title <- paste0(page_title , " profile: ", name)
    
    filtered_tables <- generate_tables(filtered_data)
    
    samples <- list(
      all = nrow(filtered_data),
      coders = sum(filtered_data$code_freq != "Never"),
      heard_of_rap = sum(filtered_data$RAP_heard_of == "Yes"),
      code_outside_current_role = sum(filtered_data$code_experience == "Yes"),
      any_code_experience = sum(filtered_data$code_experience == "Yes" | filtered_data$code_freq != "Never")
    )
    
    knitr::opts_chunk$set(warning = FALSE)
    
    rmarkdown::render(template_path, 
                      output_file = paste0(output_folder, "/", file_path),
                      quiet = TRUE,
                      params = list(
                        title = title, 
                        tables = filtered_tables,
                        samples = samples
                      ))
  } 
}
