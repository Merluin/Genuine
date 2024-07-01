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
load("data/dataset.RData") 

data <- dataset %>%
  filter(session == 1, 
         file == "1_rg_2.mp4",
         participant != 8, participant != 11, participant != 19, 
         participant != 30) %>%
  select( group, participant, gender, eta) %>%
  mutate(gender = ifelse(gender == "m", "male", "female")) # Recode gender for clarity


# Group-wise Calculations
#   - Objective: Calculate mean age, standard deviation, and count by gender for each group
group_stats <- data %>%
  group_by(group) %>%
  summarise(
    age_mean = mean(eta, na.rm = TRUE), # Mean age per group
    age_sd = sd(eta, na.rm = TRUE), # Standard deviation of age per group
    count = n(), # Total count of subjects per group
    female = sum(gender == "female"), # Count of female subjects per group
    male = sum(gender == "male") # Count of male subjects per group
  ) %>%
  ungroup()

# Overall Calculations
#   - Objective: Compute overall statistics for the entire dataset
total_stats <- data %>%
  summarise(
    group = "total",
    age_mean = mean(eta, na.rm = TRUE), # Overall mean age
    age_sd = sd(eta, na.rm = TRUE), # Overall standard deviation of age
    count = n(), # Total count of subjects
    female = sum(gender == "female"), # Total count of female subjects
    male = sum(gender == "male") # Total count of male subjects
  )

# Combine Group-wise and Total Statistics
#   - Purpose: Create a comprehensive table summarizing demographics
descriptive_table <- rbind(group_stats, total_stats) # Combine group and total stats into one table

# Export the Data Frame to an Excel File
#   - Usage: For reporting, analysis, or sharing with collaborators
write.xlsx(descriptive_table, "data/Table1.xlsx", rowNames = FALSE) # Save the table as an Excel file


# 1. ANOVA on demography
# 1. ANOVA on Group
eta <- aov_ez("participant", "eta", data, between = "group")
eta2(eta)


# Normality Test for the entire RT distribution using Shapiro-Wilk test
# Homogeneity of Variances Test using Levene's Test
residuals <- residuals(eta)
shapiro_test_residuals <- shapiro.test(residuals)
leveneTest(eta ~ group, data = data)

# QQ plot for visual inspection of residuals normality
qqnorm(residuals)
qqline(residuals, col = "steelblue")
test<-t.test(eta ~ group, data = data)
t_to_d(test$statistic,test$parameter, paired = FALSE, ci = 0.95, alternative = "two.sided")

gender_table <- table(data$group, data$gender)
chisq.test(gender_table)

#################################################
# 
# END
#
#################################################
#  Script for Genuine Study - Demographic Analysis 
#################################################