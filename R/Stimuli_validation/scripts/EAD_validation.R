###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        05/03/2024
#  Description: Authenticity Emotion Discrimination (AED)
#
#  Update:      08/09/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages
# replace_csv("session",003) pt 5 session 3 was set to 1

# Data loading
load("data/validation_dataset.RData") 

# Signal detection (SDT)
# Hits: Correct identification of genuine emotions (genuine yes response when the emotion is genuinely displayed).
# Misses: Failure to identify genuine emotions (genuine no response when the emotion is genuinely displayed).
# False Alarms: Incorrectly identifying non-genuine emotions as genuine (genuine yes response when the emotion is not genuinely displayed).
# Correct Rejections: Correctly identifying non-genuine emotions (genuine no response when the emotion is not genuinely displayed).

# HR = Hits / (Hits + Misses)
# FAR = False Alarms / (False Alarms + Correct Rejections)
# d' = Z(HR) - Z(FAR), where Z() is the z-score transformation.
# c = -0.5 * [Z(HR) + Z(FAR)], which represents the response bias.


# Model Fitting d prime
anova_d <- aov_ez("Pt.id", "d_prime", SDT_genuine, within = c("File.emotion"))

# Model Fitting criterion
anova_c <- aov_ez("Pt.id", "c", SDT_genuine, within = c("File.emotion"))
mainc_emotion <- emmeans(anova_c, pairwise ~ File.emotion)


  

# plots
data_genuine_mean <- SDT_genuine %>%
  group_by(File.emotion) %>%
  summarise(d_prime = mean(d_prime, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>%
  ungroup() 

plot_d <- data_genuine_mean %>%
  ggplot(aes(x = File.emotion, y = d_prime, group = File.emotion, color = File.emotion)) +
  geom_point(size = 4) +
  geom_line() +
  stat_summary(fun = mean, geom = "line") +  # Adjust this as needed
  coord_fixed(ylim = c(0.5, 2)) +
  labs(x = "", y = "Sensitivity (dprime)") +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
  )+
  scale_color_manual(values = c("Anger" = "#D9A9CF", "Fearfull" = "#7E5084", "Happiness" = "#4A7F99"))
  

plot_c <- data_genuine_mean %>%
  ggplot(aes(x = File.emotion, y = c, group = File.emotion, color = File.emotion)) +
  geom_point(size = 4) +
  geom_line() +
  geom_hline(yintercept = 0, color = "darkgray", size = 1) +  # Add black line at y = 0
  stat_summary(fun = mean, geom = "line") +  # Adjust this as needed
  coord_fixed(ylim = c(-0.4, 0.4)) +
  labs(x = "", y = "Bias (criterion)") +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_color_manual(values = c("Anger" = "#D9A9CF", "Fearfull" = "#7E5084", "Happiness" = "#4A7F99")) +
  ggsignif::geom_signif(
    xmin = 1, xmax = 3, y_position = 0.3, textsize = 5, color = "black",
    annotations = c("*"), tip_length = 0) 

print(plot_c)
print(plot_d)

plot_sd <- cowplot::plot_grid(
  plot_d, plot_c, #plot_c, plot_d,
  nrow = 1,  # Use nrow to specify rows instead of rows
  align = 'vh',  # Align vertically
  axis = 'lr',
  labels = "AUTO" # Align left and right axes
)
 
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################