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

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis
#################################################