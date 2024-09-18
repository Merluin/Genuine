###########################################################################
#
#  Experiment:  ccPAS
#  Programmer:  QUETTIER THOMAS
#  Date:        18/12/23
#  Description: usefull fonctions 
#
#  Update:      18/12/23
###########################################################################



#' convert_column
#' @description switch decimal , from ecxel to .
#' @param file a data.frame column
#'
#' @export
#'
convert_column <- function(column) {
  # Replace comma with dot and convert to numeric
  as.numeric(gsub(",", ".", column))
}


#' identify_outliers
#' @description identify_outliers (>2 SD) from column mean
#' @param file a data.frame column
#'
#' @export
#'
identify_outliers <- function(data, participant_column, variable_column) {
  # Calculate mean and standard deviation for the variable
  mean_value <- mean(data[[variable_column]], na.rm = TRUE)
  sd_value <- sd(data[[variable_column]], na.rm = TRUE)
  
  # Define the threshold for outliers
  upper_threshold <- mean_value + 2 * sd_value
  lower_threshold <- mean_value - 2 * sd_value
  
  # Identify participants who are outliers
  outlier_participants <- data[data[[variable_column]] > upper_threshold | data[[variable_column]] < lower_threshold, participant_column]
  
  # Return the list of outlier participants
  return(outlier_participants)
}

#' flex_ez
#' @description create a flextable from aov_ez
#' @param object a data.frame
#'
#' @export
#'
flex_ez <- function(data, title) {
  ft <- data %>%
    mutate(across(where(is.numeric), round, digits = 3),
           level = row.names(.)) %>%
    select(level, everything()) %>%
    flextable() %>%
    autofit() %>%
    theme_vanilla() %>%
    fontsize(part = "all", size = 7) %>%
    set_caption(title) %>%
    set_header_labels(values = list(level = ""))
  
  return(ft)
}

#' flex_emmeans
#' @description create a flextable from emmeans
#' @param object a $contrast
#'
#' @export
#'
flex_emmeans <- function(data, title) {
  ft <- data %>%
    data.frame() %>%
    mutate(across(where(is.numeric), round, digits = 3)) %>%
    flextable() %>%
    autofit() %>%
    theme_vanilla() %>%
    fontsize(part = "all", size = 7) %>%
    set_caption(title) %>%
    set_header_labels(values = list(level = ""))
  
  return(ft)
}


#' run_script
#' @description source a script and return a nice message
#' @param file a character indicating the file path and name
#'
#' @export
#'
run_script <- function(file){
  
  file_name <- basename(file)
  file_name <- deparse(substitute(file_name))
  
  clean_env() # clean everything
  
  source(file)
  
  success(paste(file_name, "finished!"))
  
}


#' clean_env
#' @description clean the global environment
#' @export
#'
clean_env <- function(){
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

#' success
#'
#' @param msg a character indicating the message to be printed
#'
#' @export
#'
success <- function(msg){
  cli::cli_alert_success(msg)
}