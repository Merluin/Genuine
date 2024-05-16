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

# Signal detection (SDT)
# Hits: Correct identification of genuine emotions (genuine yes response when the emotion is genuinely displayed).
# Misses: Failure to identify genuine emotions (genuine no response when the emotion is genuinely displayed).
# False Alarms: Incorrectly identifying non-genuine emotions as genuine (genuine yes response when the emotion is not genuinely displayed).
# Correct Rejections: Correctly identifying non-genuine emotions (genuine no response when the emotion is not genuinely displayed).

# HR = Hits / (Hits + Misses)
# FAR = False Alarms / (False Alarms + Correct Rejections)

# d' = Z(HR) - Z(FAR), where Z() is the z-score transformation.
# c = -0.5 * [Z(HR) + Z(FAR)], which represents the response bias.
# b = (ZHR^2 - ZFAR^2 ) / 2


# Calculating hits, misses, false alarms, and correct rejections
data_sdt <- dataset_pulito %>%
  filter(Trials_loop.thisRepN == 0) %>%
  mutate(sdt = case_when(
    elicitation == "genuine" & gesino_kb.corr == 1 ~ "Hits",
    elicitation == "genuine" & gesino_kb.corr == 0 ~ "Misses",
    elicitation == "paused" & gesino_kb.corr == 0 ~ "False.Alarms",
    elicitation == "paused" & gesino_kb.corr == 1 ~ "Correct.Rejections"
  ),
  count = 1) %>%
  group_by(participant, emotion, sdt) %>%
  summarise(count = sum(count), .groups = 'drop') %>%
  pivot_wider(names_from = sdt, values_from = count, values_fill = list(count = 0)) %>%
  mutate(HR = Hits / (Hits + Misses),
         FAR = False.Alarms / (False.Alarms + Correct.Rejections))

# Applying correction for HR and FAR of 1 or 0
data_sdt <- data_sdt %>%
  mutate(HR = ifelse(HR == 1, 1 - 1/(2 * (Hits + Misses)), ifelse(HR == 0, 1/(2 * (Hits + Misses)), HR)),
         FAR = ifelse(FAR == 1, 1 - 1/(2 * (False.Alarms + Correct.Rejections)), ifelse(FAR == 0, 1/(2 * (False.Alarms + Correct.Rejections)), FAR)))

# Calculate Z-scores for HR and FAR
data_sdt <- data_sdt %>%
  mutate(ZHR = qnorm(HR),
         ZFAR = qnorm(FAR),
         d_prime = ZHR - ZFAR,
         c = -0.5 * (ZHR + ZFAR),
         b = (ZHR^2 - ZFAR^2)/2)


  
# Summarizing the data
summaryz <- data_sdt %>%
  group_by(emotion) %>%
  summarise(across(c(d_prime, c, b), list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))), .groups = 'drop')
  

# Model Fitting
anova_d <- aov_ez("participant", "d_prime", data_sdt, within = c("emotion"))
posthoc_d <- emmeans(anova_d, pairwise ~ emotion)

anova_c <- aov_ez("participant", "c", data_sdt, within = c("emotion"))
posthoc_c <- emmeans(anova_c, pairwise ~ emotion)

anova_b <- aov_ez("participant", "b", data_sdt, within = c("emotion"))
posthoc_b <- emmeans(anova_b, pairwise ~ emotion)

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis
#################################################