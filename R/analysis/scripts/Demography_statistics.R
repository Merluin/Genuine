###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        20/05/2024
#  Description: demography information
#
#  Update:      08/09/2024
###########################################################################

# Clearing workspace
rm(list = ls())  # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all()  # Load necessary functions and packages

# Data loading and eliminate outliers and 5 of the 6 M1 group
# Data loading
load("data/psychopy_dataset.RData") 

demography <- demography %>%
  filter( Pt.group != "sham") %>%
  mutate(Pt.group = factor(Pt.group, levels = c("Exp1rpSTS-rIFG", "Ctrl1rIFG-rpSTS", "Exp2rIFG-rM1", "Ctrl2rM1-rIFG")))


# Group-wise Calculations
#   - Objective: Calculate mean age, standard deviation, and count by gender for each group
group_stats <- demography %>%
  group_by(Pt.group) %>%
  summarise(
    age_mean = mean(Pt.age, na.rm = TRUE), # Mean age per group
    age_sd = sd(Pt.age, na.rm = TRUE), # Standard deviation of age per group
    count = n(), # Total count of subjects per group
    female = sum(Pt.gender == "female"), # Count of female subjects per group
    male = sum(Pt.gender == "male") # Count of male subjects per group
  ) %>%
  ungroup()

# Overall Calculations
#   - Objective: Compute overall statistics for the entire dataset
total_stats <- demography %>%
  summarise(
    Pt.group = "total",
    age_mean = mean(Pt.age, na.rm = TRUE), # Overall mean age
    age_sd = sd(Pt.age, na.rm = TRUE), # Overall standard deviation of age
    count = n(), # Total count of subjects
    female = sum(Pt.gender == "female"), # Total count of female subjects
    male = sum(Pt.gender == "male") # Total count of male subjects
  )

# Combine Group-wise and Total Statistics
#   - Purpose: Create a comprehensive table summarizing demographics
descriptive_table <- rbind(group_stats, total_stats) # Combine group and total stats into one table


# 1. ANOVA on demography
# 1. ANOVA on Group
eta <- aov_ez("Pt.id", "Pt.age", demography, between = "Pt.group")
print(eta)
eta2(eta)


# Normality Test for the entire RT distribution using Shapiro-Wilk test
# Homogeneity of Variances Test using Levene's Test
residuals <- residuals(eta)
shapiro_test_residuals <- shapiro.test(residuals)
leveneTest(Pt.age ~ Pt.group, data = demography)

# QQ plot for visual inspection of residuals normality
qqnorm(residuals)
qqline(residuals, col = "steelblue")

gender_table <- table(demography$Pt.group, demography$Pt.gender)
chisq.test(gender_table)

#################################################
# 
# END
#
#################################################
#  Script for Genuine Study - Demographic Analysis 
#################################################