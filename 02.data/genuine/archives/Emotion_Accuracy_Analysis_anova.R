###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy analysis emotion
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
anova_acc <- aov_ez("participant", "emotion.acc.mean", dataset, within = c("emotion", "elicitation"))

# To look at pairwise comparisons within each main effects
main_emotion <- emmeans(anova_acc, pairwise ~ emotion)
main_elicitation <- emmeans(anova_acc, pairwise ~ elicitation)




#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################