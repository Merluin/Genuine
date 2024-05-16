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
  GenuineDetection() %>%
  mutate(Session = as.factor(session),
         Emotion = as.factor(emotion),
         Group = as.factor(group),
         Participant = participant)

#write.csv(data_genuine, "data/signaldetection.csv", row.names=FALSE)


# Model Fitting
anova_d <- aov_ez("participant", "d_prime", data_genuine, within = c("emotion","session"), between = "group")
posthoc_d <- emmeans(anova_d, pairwise ~ session|emotion)

# plots
pt_plot <- data_genuine  %>%
  ggplot(aes(x = Session, y = d_prime, group = Participant, color = Participant)) +
  geom_point() +
  geom_line() +
  stat_summary() +
  facet_grid(Emotion ~ Group) +
  theme_minimal() + theme(legend.position="none")

# plots
data_genuine_mean <- data_genuine %>%
  group_by(Emotion, Session, Group) %>%
  summarise(d_prime = mean(d_prime, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>%
  ungroup()

plot_d <- data_genuine_mean %>%
  ggplot(aes(x = Session, y = d_prime ,group=Group, color = Group)) +
  geom_point() +
  geom_line() +
  stat_summary() +
  facet_grid(Emotion ~ .) +
  coord_fixed(ylim = c(0, 2))+
  theme_minimal()+ theme(legend.position="none")

plot_c <- data_genuine_mean %>%
  ggplot(aes(x = Session, y = c ,group=Group, color = Group)) +
  geom_point() +
  geom_line() +
  stat_summary() +
  facet_grid(Emotion ~ .) +
  coord_fixed(ylim = c(-0.5, 1))+
  theme_minimal()+ theme(legend.position="bottom")

plot_sd <- cowplot::plot_grid(plot_d, plot_c, labels = "AUTO")


#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################