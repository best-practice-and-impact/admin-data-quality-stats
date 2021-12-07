#'@title Enforce streaming rules
#'
#'@description Input the data as a data frame and apply each of the streaming rules in turn.
#'
#'@param data the data which is the output from carsurvey2::tidy_ingest(data) 
#'
#'@return the exported data after streaming rules as a dataframe
#'
#'@export

enforce_streaming <- function(data){
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  } 
  
  #If no degree then don't ask page 3
  Q6_data <- dplyr::select(data,"Q7":"Q7.16")
  Q6_data[!is.na(data$Q6) & !(data$Q6 %in% c("Bachelor's degree (or equivalent)","Master's degree (or equivalent)","Doctoral degree (or equivalent)")),] <- NA
  data[colnames(Q6_data)] <- Q6_data
  
  #If Q8 is "Never" then make P12 NA
  Q8_data <- dplyr::select(data,"Q19":"Q24")
  Q8_data[!is.na(data$Q8) & data$Q8 == "Never",] <- NA
  data[colnames(Q8_data)] <- Q8_data
  
  # if Q11 == "No"
  Q11_data <- dplyr::select(data,"Q12":"Q14")
  Q11_data[!is.na(data$Q11) & data$Q11 == "No",] <- NA
  data[colnames(Q11_data)] <- Q11_data
  
  # if Q13 "No" skip P7
  data$Q14 <- ifelse(!is.na(data$Q13) & data$Q13 =="Yes", data$Q14, NA)
  
  #If Q15 "No" skip P 9, 10 and 11
  Q15_data <- dplyr::select(data,"Q16":"Q18.6")
  Q15_data[!is.na(data$Q15) & data$Q15 == "No",] <- NA
  data[colnames(Q15_data)] <- Q15_data
  
  #If Q16 is "Yes" Skip P10
  data$Q17 <- ifelse(!is.na(data$Q16) & data$Q16 =="Yes" , NA, data$Q17)
  
  #If Q19: select source code and I don't understand ot never then skip P13
  Q19_data <- dplyr::select(data,"Q25":"Q25.5")
  Q19_data[!is.na(data$Q19.3) & data$Q19.3 %in% c("Never","I don't understand this question"),] <- NA
  data[colnames(Q19_data)] <- Q19_data

  return(data)
}
