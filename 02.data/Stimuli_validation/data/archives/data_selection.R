###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy analysis
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages


# Data loading
load("data/validation.RData") 

# Model Fitting
anova_acc <- aov_ez("participant", "genuine.acc.mean", summary_participants, within = c("emotion","elicitation"))

# To look at pairwise comparisons within each emotion for different elicitation levels
interaction_pairwise <- emmeans(anova_acc, pairwise ~ emotion | elicitation)

# Model Fitting only EMOTION
anova_acc <- aov_ez("participant", "genuine.acc.mean", summary_participants, within = c("emotion"))
postHoc <- emmeans(anova_acc, pairwise ~ emotion)

# Initialize a data frame to store the results
exclusion_results <- data.frame(
  subject_excluded_1 = character(), 
  subject_excluded_2 = character(), 
  p_value_emotion = numeric(), 
  p_value_elicitation = numeric(), 
  p_value_interaction = numeric(), 
  stringsAsFactors = FALSE
)

# Get a list of all unique participants
participant_list <- unique(summary_participants$participant)

# Generate all combinations of two participants to be excluded
participant_combinations <- combn(participant_list, 2)

# Loop through each combination of two participants
for (i in 1:ncol(participant_combinations)) {
  
  # Exclude the current combination of two participants from the analysis
  excluded_participants <- participant_combinations[, i]
  subset_data <- summary_participants[!summary_participants$participant %in% excluded_participants, ]
  
  # Run the ANOVA without the current combination of two participants
  anova_acc <- aov_ez("participant", "genuine.acc.mean", subset_data, within = c("emotion", "elicitation"))
  
  # Extract the p-value for the elicitation factor
  elicitation_p_value <- anova_acc$anova_table$`Pr(>F)`[2]
  
  # Store the results
  exclusion_results <- rbind(exclusion_results, data.frame(
    subject_excluded_1 = excluded_participants[1], 
    subject_excluded_2 = excluded_participants[2], 
    p_value_emotion = anova_acc$anova_table$`Pr(>F)`[1],
    p_value_elicitation = anova_acc$anova_table$`Pr(>F)`[2],
    p_value_interaction = anova_acc$anova_table$`Pr(>F)`[3]
  ))
}

# Find the combination that yields the maximum p_value_elicitation
max_combination_elicitation <- which.max(exclusion_results$p_value_elicitation)
max_combination_emotion <- which.max(exclusion_results$p_value_emotion)
max_combination_interaction <- which.max(exclusion_results$p_value_interaction)

exclusion_results[c(max_combination_emotion,
                    max_combination_elicitation,
                    max_combination_interaction),]

range(exclusion_results$p_value_interaction)
range(exclusion_results$p_value_emotion)
range(exclusion_results$p_value_elicitation)

unique(summary_participants$participant)

identify_outliers(summary_participants,"participant","genuine.acc.mean")

# 12, 14

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis
#################################################