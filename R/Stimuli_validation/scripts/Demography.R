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

# Data loading
load("data/validation_dataset.RData") 


# Group-wise Calculations
#   - Objective: Calculate mean age, standard deviation, and count by gender for each group
group_stats <- demography %>%
  summarise(
    age_mean = mean(Pt.age, na.rm = TRUE), # Mean age per group
    age_sd = sd(Pt.age, na.rm = TRUE), # Standard deviation of age per group
    count = n(), # Total count of subjects per group
    female = sum(Pt.gender == "female", na.rm = TRUE), # Count of female subjects per group
    male = sum(Pt.gender == "male", na.rm = TRUE) # Count of male subjects per group
  )



gender_table <- table( demography$Pt.gender)
chisq.test(gender_table)

#################################################
# 
# END
#
#################################################
#  Script for Genuine Study - Demographic Analysis 
#################################################