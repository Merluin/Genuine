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
load("data/psychopy_dataset.RData") 

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
#ANOVA for baseline
dat_session1 <- SDT_genuine %>%
  filter(Exp.session == "baseline")

anova_d_baseline <- aov_ez("Pt.id", "d_prime", dat_session1, within = "File.emotion", between = "Pt.group")

# full model
 
anova_d <- aov_ez("Pt.id", "d_prime", SDT_genuine, within = c("File.emotion","Exp.session"), between = "Pt.group")

interaction_d <- emmeans(anova_d, pairwise ~ Exp.session|Pt.group)
maind_session <- emmeans(anova_d, pairwise ~ Exp.session)
maind_emotion <- emmeans(anova_d, pairwise ~ File.emotion)

emm_df <- as.data.frame(interaction_d) %>%
  filter(Exp.session != ".")
ggplot(emm_df, aes(x = Exp.session, y = emmean, color = Pt.group, group = Pt.group)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.1)) +
  labs(title = "Interaction Between Session and Group", 
       x = "Experimental Session", 
       y = "Estimated Marginal Means of d_prime") +
  theme_minimal()


# Model Fitting criterion
#ANOVA for baseline
anova_c_baseline <- aov_ez("Pt.id", "c", SDT_genuine, within = "File.emotion", between = "Pt.group")

anova_c <- aov_ez("Pt.id", "c", SDT_genuine, within = c("File.emotion","Exp.session"), between = "Pt.group")
mainc_emotion <- emmeans(anova_c, pairwise ~ File.emotion)


# plots
data_genuine_mean <- SDT_genuine %>%
  group_by(File.emotion, Exp.session, Pt.group) %>%
  summarise(d_prime = mean(d_prime, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>%
  ungroup() 

x_breaks <- c("baseline", "T0", "T20")


plot_d <- data_genuine_mean %>%
  group_by(Exp.session, Pt.group) %>%
  summarise(d_prime = mean(d_prime)) %>%
  ungroup() %>%
  ggplot(aes(x = Exp.session, y = d_prime, group = Pt.group, color = Pt.group)) +
  geom_point() +
  geom_line() +
  stat_summary(fun = mean, geom = "line") +  # Adjust this as needed
  coord_fixed(ylim = c(0.8, 2)) +
  labs(x = "", y = "Sensitivity (dprime)") +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_x_discrete(breaks = x_breaks) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
  )+
  scale_color_manual(values = c("Ctrl1rIFG-rpSTS" = "#D9A9CF", "Ctrl2rpSTS-rIFG" = "#7E5084", "Exp1rIFG-rM1" = "#4A7F99"))+
  ggsignif::geom_signif(
    xmin = 1, xmax = 2, y_position = 1.6, textsize = 5, color = "#4A7F99",
    annotations = c("**"), tip_length = 0) +
  ggsignif::geom_signif(
    xmin = 1, xmax = 3, y_position = 1.8, textsize = 5, color = "#4A7F99",  # Lowered the y_position from 1.9 to 1.85
    annotations = c("***"), tip_length = 0)

 plot_c <- data_genuine_mean %>%
   group_by(Exp.session, Pt.group) %>%
   summarise(c = mean(c)) %>%
   ungroup() %>%
   ggplot(aes(x = Exp.session, y = c, group = Pt.group, color = Pt.group)) +
   geom_point() +
   geom_line() +
   stat_summary(fun = mean, geom = "line") +  # Adjust this as needed
   coord_fixed(ylim = c(-0.4, 0.4)) +
   labs(x = "", y = "Bias (criterion)") +
   theme_minimal() + 
   theme(legend.position = "none") +
   scale_x_discrete(breaks = x_breaks) +
   scale_color_manual(values = c("Ctrl1rIFG-rpSTS" = "#D9A9CF", "Ctrl2rpSTS-rIFG" = "#7E5084", "Exp1rIFG-rM1" = "#4A7F99"))+
   theme(
     aspect.ratio = 1/2 ) # Adjust aspect ratio to make the 
   
 print(plot_c)
 print(plot_d)

 plot_sd <- cowplot::plot_grid(
   plot_d, plot_c, #plot_c, plot_d,
   nrow = 2,  # Use nrow to specify rows instead of rows
   align = 'vh',  # Align vertically
   axis = 'lr',
   labels = "AUTO" # Align left and right axes
 )
 
   
 # Running ANCOVA for d_delta
 ancova_d_delta <- aov(d_delta ~ Pt.group + fantasy + perspective_taking + empathic_concern + personal_distress, 
                       data = ΓAED)
 summary(ancova_d_delta)
 
 ancova_model <- aov(d_delta ~ Pt.group + iri_tot + tas_tot, data = ΓAED)
 summary(ancova_model)
 
 # Running ANCOVA for c_delta
 ancova_c_delta <- aov(c_delta ~ Pt.group + fantasy + perspective_taking + empathic_concern + personal_distress, 
                       data = ΓAED)
 summary(ancova_c_delta)

 ancova_model <- aov(c_delta ~ Pt.group + iri_tot + tas_tot, data = ΓAED)
 summary(ancova_model)
 
 
 
 # slider analysis
 
 data <- dataset %>% 
   filter(EIJ.accuracy == 1) %>%
   group_by(Pt.id,File.emotion,Exp.session,Pt.group) %>%
   summarise(slider = abs(mean(EIJ.intensity, na.rm = TRUE)))
 
anova <- aov_ez("Pt.id", "slider", data, within = c("File.emotion","Exp.session"), between = "Pt.group")
 
 main_emotion <- emmeans(anova, pairwise ~ File.emotion)
 main_Session <- emmeans(anova, pairwise ~ Exp.session)
 
 inter_emotion_session <- emmeans(anova, pairwise ~  File.emotion | Exp.session)
 inter_session_emotion <- emmeans(anova, pairwise ~   Exp.session | File.emotion)
 
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################