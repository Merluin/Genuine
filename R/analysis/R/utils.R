###########################################################################
#
#  Experiment:  ccPAS
#  Programmer:  QUETTIER THOMAS
#  Date:        18/12/23
#  Description: usefull fonctions 
#
#  Update:      18/12/23
###########################################################################
#' eta2
#' @description return eta2 from anova aov_ez
#' @param fit from aov_ez
#'
#' @export
#'
eta2 <- function(anova) {
  # Extract the ANOVA table
  anova_table <- anova$anova_table
  # Calculate the sum of squares for the effect and residuals
  ss_effect <- anova_table$`num Df` * anova_table$`MSE` * anova_table$`F`
  ss_residual <- anova_table$`den Df` * anova_table$`MSE`
  ss_total <- ss_effect + ss_residual
  # Compute eta squared for the effect
  eta2 <- ss_effect / ss_total
  
  # Combine eta squared with the ANOVA table
  eta_squared_table <- data.frame(
    Term = rownames(anova_table),  # Exclude residual term
    Eta_Squared = eta2)
  
  return(eta_squared_table)
}


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

#' replace_csv
#' @description replace csv and a column in csv 
#' @param file a .csv
#'
#' @export
#'
replace_csv <- function(targetcolumn, valueIwant) {
  # Prompt the user to choose a file
  filepath <- file.choose()
  
  # Read the CSV file
  data <- read.csv(filepath)
  
  # Check if the target column exists
  if(!targetcolumn %in% colnames(data)) {
    stop("The specified column does not exist in the CSV file.")
  }
  
  # Replace the values in the target column
  data[[targetcolumn]] <- valueIwant
  
  # Write the modified data frame back to the original file
  write.csv(data, filepath, row.names = FALSE)
  
  cat("The values in column", targetcolumn, "have been replaced with", valueIwant, "\nFile saved at:", filepath)
}

#' add_to_csv
#' @description add column to csv 
#' @param file a .csv
#'
#' @export
#'
add_to_csv <- function(newcolumn, valueIwant) {
  # Prompt the user to choose a file
  filepath <- file.choose()
  
  # Read the CSV file
  data <- read.csv(filepath)
  
  # Check if the target column exists
 
  
  # Replace the values in the target column
  data[[newcolumn]] <- valueIwant
  
  # Write the modified data frame back to the original file
  write.csv(data, filepath, row.names = FALSE)
  
}

#' add_pt_row_means
#' @description had hoc function to compute mean row of participants
#' @param dataframe a data.frame
#'
#' @export
#'
add_pt_row_means <- function(df) {
  pt_cols_indices <- grep("^pt", names(df), value = FALSE)
  df$row_mean <- rowMeans(df[, pt_cols_indices], na.rm = TRUE)
  return(df)
}


#' create_emotion_df
#' @description create a templet data.frame()
#' @param chr a group name
#'
#' @export
#'
create_emotion_df <- function(group_name) {
  data.frame(
    Emotion = rep(c("Anger", "Fearfull", "Happyness"), each = 9),
    Session = rep(c(1, 2, 3), each = 3, 3),
    Elicitation = rep(c(  "Genuine", "Paused", "Total"), 9),
    Group = rep(group_name, 27)
  )
}

#' create_bar_plot
#' @description create a ggplot
#' @param 
#'
#' @export
#'
create_bar_plot <- function(data) {
  ggplot( data, aes(x = emotion, y = mean, fill = session)) +
  geom_bar(position = position_dodge(width = 0.85), stat = "identity", color = "black") +  # Bar plot with mean values
  geom_errorbar(aes(ymin = mean, ymax = mean + se),
                position = position_dodge(width = 0.85), width = 0.25) +
  # facet_grid(. ~ group) +
  #coord_flip(ylim = c(0.6, 0.9)) + 
  coord_cartesian(ylim = c(0.5, 1)) +# Extend the y-axis limits
  labs(x = "", y = "Authenticity Emotion Discrimination (%)", fill = "Session") +  # Labels for axes and legend
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add axis lines back
    axis.ticks = element_line(color = "black"),  # Add axis ticks back
    text = element_text(family = "Helvetica"),
   # axis.text.x = element_text(angle = 45, hjust = 1) # 
  )}

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