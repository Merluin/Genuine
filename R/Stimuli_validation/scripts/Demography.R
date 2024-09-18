###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        20/05/2024
#  Description: demography
#
#  Update:      20/05/2024
###########################################################################

# Clearing workspace
rm(list = ls())  # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all()  # Load necessary functions and packages

# Data loading and eliminate outliers and 5 of the 6 M1 group
# Data loading
# Data loading
load("data/validation_dataset.RData") 


# Group-wise Calculations
#   - Objective: Calculate mean age, standard deviation, and count by gender for each group
group_stats <- data %>%
  summarise(
    age_mean = mean(eta, na.rm = TRUE), # Mean age per group
    age_sd = sd(eta, na.rm = TRUE), # Standard deviation of age per group
    count = n(), # Total count of subjects per group
    female = sum(gender == "female"), # Count of female subjects per group
    male = sum(gender == "male") # Count of male subjects per group
  ) 


# Export the Data Frame to an Excel File
#   - Usage: For reporting, analysis, or sharing with collaborators
write.xlsx(group_stats, "data/Table1.xlsx", rowNames = FALSE) # Save the table as an Excel file


gender_table <- table( data$gender)
chisq.test(gender_table)

#################################################
# 
# END
#
#################################################
#  Script for Genuine Study - Demographic Analysis 
#################################################