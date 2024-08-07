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
dataset <- dataset %>% 
  filter(
    id != 8, id != 11, id != 19, 
    id != 30)

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
  gather(signal, score, 5:8) %>%
  mutate(score = factor(score),
         emotion = factor(emotion),
         session = factor(session),
         group = factor(group),
         signal = factor(signal, ordered = TRUE))

write.csv(data_genuine, file = "~/Desktop/data_genuine.csv", row.names = FALSE)

# Fit the cumulative link model
model <- clm(score ~ emotion * session * group * signal, data = data_genuine)

# Summary of the model
summary(model)

# ANOVA-like table for interpreting results
anova_results <- Anova(model, type = "III")
posthoc <- emmeans(model, pairwise ~ emotion | session | group | signal)
summary(posthoc)

  
  # mutate(Session = as.factor(session),
  #        Emotion = as.factor(emotion),
  #        Group = as.factor(group),
  #        Participant = participant,
  #         HR = abs(Hits)/( abs(Hits)+abs(Misses) ),
  #         FAR = abs(False.Alarms)/( abs(False.Alarms)+abs(Correct.Rejections) ))

# Calculate Z-scores for HR and FAR
data_sdt <- data_genuine %>%
  mutate(ZHR = qnorm(HR),
         ZFAR = qnorm(FAR),
         d_prime = ZHR - ZFAR,
         c = -0.5 * (ZHR + ZFAR))
#write.csv(data_genuine, "data/signaldetection.csv", row.names=FALSE)
anova_d <- aov_ez("participant", "d_prime", data_sdt, within = c("Emotion","Session"), between = "Group")


# Model Fitting
anova_hits <- aov_ez("participant", "Hits", data_genuine, within = c("Emotion","Session"), between = "Group")

main_emotion <- emmeans(anova_hits, pairwise ~ Emotion)
main_Session <- emmeans(anova_hits, pairwise ~ Session)
inter <- emmeans(anova_hits, pairwise ~ Session|Emotion)


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