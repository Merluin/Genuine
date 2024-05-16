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

# Genuine
# Model Fitting
anova_acc <- aov_ez("participant", "genuine.acc.mean", summary_participants, within = c("emotion","elicitation"))
interaction_pairwise <- emmeans(anova_acc, pairwise ~ emotion | elicitation)

anova_genuine <- flex_ez(anova_acc$anova_table,"ANOVA Genuine Accuracy")
interaction_genuine <- flex_emmeans(interaction_pairwise$contrasts, "Interaction Emotion * Elicitation")
  
# Emotion
# Model Fitting
anova_acc <- aov_ez("participant", "emotion.acc.mean", summary_participants, within = c("emotion","elicitation"))
main_emotion <- emmeans(anova_acc, pairwise ~ emotion)
anova_emotion <- flex_ez(anova_acc$anova_table,"ANOVA Emotion Accuracy")
main_emotion <- flex_emmeans(main_emotion$contrasts, "Main Effect Emotion")

# Creating a Word document with the table
doc <- read_docx()
doc <- body_add_flextable(doc, value = table)
doc <- body_add_par(doc, value = "")
doc <- body_add_par(doc, value = "")
doc <- body_add_flextable(doc, value = anova_genuine)
doc <- body_add_flextable(doc, value = interaction_genuine)
doc <- body_add_par(doc, value = "")
doc <- body_add_par(doc, value = "")
doc <- body_add_flextable(doc, value = anova_emotion)
doc <- body_add_flextable(doc, value = main_emotion)
file_path <- "objects/Statistica.docx"
print(doc, target = file_path)

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis
#################################################