###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        05/03/2024
#  Description: Pilot signal detection
#
#  Update:      05/03/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages
# replace_csv("session",003) pt 5 session 3 was set to 1

# Data loading
load("data/dataset.RData") 

#write.csv(data, "data/data.csv", row.names=FALSE)

# Signal detection (SDT)
# Hits: Correct identification of genuine emotions (genuine yes response when the emotion is genuinely displayed).
# Misses: Failure to identify genuine emotions (genuine no response when the emotion is genuinely displayed).
# False Alarms: Incorrectly identifying non-genuine emotions as genuine (genuine yes response when the emotion is not genuinely displayed).
# Correct Rejections: Correctly identifying non-genuine emotions (genuine no response when the emotion is not genuinely displayed).

# HR = Hits / (Hits + Misses)
# FAR = False Alarms / (False Alarms + Correct Rejections)

# d' = Z(HR) - Z(FAR), where Z() is the z-score transformation.
# c = -0.5 * [Z(HR) + Z(FAR)], which represents the response bias.


# Calculating hits, misses, false alarms, and correct rejections
data_genuine <- dataset %>%
  SliderGenuineDetection() %>%
  mutate(Session = as.factor(session),
         Emotion = as.factor(emotion),
         Group = as.factor(group),
         Participant = participant)
         # ,
         # HR = abs(Hits)/( abs(Hits)+abs(Misses) ),
         # FAR = abs(False.Alarms)/( abs(False.Alarms)+abs(Correct.Rejections) ))

#write.csv(data_genuine, "data/signaldetection.csv", row.names=FALSE)


# Model Fitting
anova_hits <- aov_ez("participant", "Hits", data_genuine, within = c("Emotion","Session"), between = "Group")
posthoc_d <- emmeans(anova_hits, pairwise ~ Emotion)

anova_misses <- aov_ez("participant", "Misses", data_genuine, within = c("Emotion","Session"), between = "Group")

anova_cr <- aov_ez("participant", "Correct.Rejections", data_genuine, within = c("Emotion","Session"), between = "Group")

anova_fa <- aov_ez("participant", "False.Alarms", data_genuine, within = c("Emotion","Session"), between = "Group")



#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################