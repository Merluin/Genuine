###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        05/03/2024
#  Description: Authenticity Emotion Discrimination (AED)
#
#  Update:      12/11/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages

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

sdt_data <- dataset %>%
  GenuineDetection() 

# Filter data for the baseline session
dat_session1 <- sdt_data%>%
  filter(Exp.session == "baseline")

# Perform ANOVA for sensitivity (d') at baseline
anova_d_baseline <- aov_ez("Pt.id", "d_prime", dat_session1, within = "File.emotion", between = "Pt.group")

# Perform ANOVA for full model on sensitivity (d')
anova_d <- aov_ez("Pt.id", "d_prime", sdt_data, within = c("File.emotion","Exp.session"), between = "Pt.group")

# Post-hoc comparisons for session and group interactions
interaction_d <- emmeans(anova_d, pairwise ~ Exp.session|Pt.group, adjust = "bonferroni")
maind_session <- emmeans(anova_d, pairwise ~ Exp.session)
maind_emotion <- emmeans(anova_d, pairwise ~ File.emotion)




# Perform ANOVA for response bias (c) at baseline
anova_c_baseline <- aov_ez("Pt.id", "c", dat_session1, within = "File.emotion", between = "Pt.group")

# Perform ANOVA for full model on response bias (c)
anova_c <- aov_ez("Pt.id", "c", sdt_data, within = c("File.emotion","Exp.session"), between = "Pt.group")
mainc_emotion <- emmeans(anova_c, pairwise ~ File.emotion)
interaction_c <- emmeans(anova_c, pairwise ~ Exp.session|Pt.group)

# Filter for trials with AED intensity >= 5 for meta-analysis
meta_sdt_data <- dataset %>%
  filter(abs(AED.intensity) >= 4) %>% 
  GenuineDetection() %>%
  mutate(analysis = "meta d'")

# Filter meta-analysis data for baseline session
meta_dat_session1 <- meta_sdt_data%>%
  filter(Exp.session == "baseline") 

# Perform ANOVA for sensitivity (meta d') at baseline
anova_Md_baseline <- aov_ez("Pt.id", "d_prime", meta_dat_session1, within = "File.emotion", between = "Pt.group")

# Perform ANOVA for sensitivity (meta d') for full model
anova_Md <- aov_ez("Pt.id", "d_prime", meta_sdt_data, within = c("File.emotion","Exp.session"), between = "Pt.group")

# Perform ANOVA for response bias (meta c) at baseline
anova_Mc_baseline <- aov_ez("Pt.id", "c", meta_dat_session1, within = "File.emotion", between = "Pt.group")

# Perform ANOVA for response bias (meta c) for full model
anova_Mc <- aov_ez("Pt.id", "c", meta_sdt_data, within = c("File.emotion","Exp.session"), between = "Pt.group")





# Calculate mean d' and c for each emotion, session, and group
data_genuine_mean <-sdt_data %>%
  group_by(File.emotion, Exp.session, Pt.group) %>%
  summarise(d_prime = mean(d_prime, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>%
  ungroup() 

x_breaks <- c("baseline", "T0", "T20")

# Plot for sensitivity (d')
plot_d <- as.data.frame(interaction_d) %>%
  filter(Exp.session != ".") %>%
  ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, color = "black") +
  geom_line(position = position_dodge(width = 0.3), size = 0.5, color = "black") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, position = position_dodge(width = 0.3), color = "black") +
  theme_minimal() +
  coord_fixed(ylim = c(0.5, 2.5)) +
  labs(x = "", y = "Sensitivity (dprime)") +
  theme(
    legend.position = "none", # Remove legend
    panel.grid = element_blank(), # Remove grid lines
    axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
    axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
    axis.ticks.length = unit(0.2, "cm") # Set tick length
  )+ theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
  )+
  scale_x_discrete(breaks = x_breaks)  +
  ggsignif::geom_signif(
    xmin = 1 + 0.1, xmax = 2 + 0.1, y_position = 1.75, textsize = 4, 
    size = 0.4, # Make the bar more subtle by reducing thickness
    color = "black", annotations = c("**"), tip_length = 0, 
    position = position_dodge(width = 0.1)) +
  ggsignif::geom_signif(
    xmin = 1 + 0.1, xmax = 3 + 0.1, y_position = 2.05, textsize = 4, 
    size = 0.4, # Make the bar more subtle
    color = "black", annotations = c("***"), tip_length = 0, 
    position = position_dodge(width = 0.1)) +
  ggsignif::geom_signif(
    xmin = 1 + 0.03, xmax = 2 + 0.03, y_position = 1.6, textsize = 4, 
    size = 0.4, # Subtle bar
    color = "black", annotations = c("*"), tip_length = 0, # Use superscript
    position = position_dodge(width = 0.1)) +
  ggsignif::geom_signif(
    xmin = 1 + 0.03, xmax = 3 + 0.03, y_position = 1.9, textsize = 4, 
    size = 0.4, # Subtle bar
    color = "black", annotations = c("+"), tip_length = 0, 
    position = position_dodge(width = 0.1))


# Extract the legend from the plot
plot <- as.data.frame(interaction_d) %>%
  filter(Exp.session != ".") %>%
  ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3, color = "black")+ 
  guides(shape = guide_legend(nrow = 1))
legend <- cowplot::get_legend(plot)

# Plot for response bias (c)
 plot_c <- as.data.frame(interaction_c) %>%
   filter(Exp.session != ".") %>%
   ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
   geom_point(position = position_dodge(width = 0.3), size = 3, color = "black") +
   geom_line(position = position_dodge(width = 0.3), size = 0.5, color = "black") +
   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                 width = 0.2, position = position_dodge(width = 0.3), color = "black") +
   theme_minimal() +
   coord_fixed(ylim = c(-0.4, 0.4)) +
   labs(x = "", y = "Bias (criterion)") +
   theme(
     legend.position = "none", # Remove legend
     panel.grid = element_blank(), # Remove grid lines
     axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
     axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
     axis.ticks.length = unit(0.2, "cm") # Set tick length
   ) 
   plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
 
 
 

 # Combine the two plots in one row
 plots_row <- cowplot::plot_grid(
   plot_d, plot_c, 
   ncol = 1, 
   rel_widths = c(1, 1), # Ensure equal widths for both plots
   align = "vh" # Align vertically and horizontally to make them the same size
 )
 
 
 # Combine the plots row with the legend
 final_plot <- cowplot::plot_grid(
   plots_row, # Add the row of plots
   legend, # Add the legend below
   ncol = 1, # Combine vertically
   rel_heights = c(1, 0.1) # Adjust relative heights (legend closer to the plots)
 )
 
 # Print the final plot
 print(final_plot)
 
 # Running ANCOVA for d_delta IRI
 ancova_d_delta <- aov(d_delta ~ Pt.group * (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = ΓAED)
 summary(ancova_d_delta)
main_group <- emmeans(ancova_d_delta, pairwise ~ Pt.group)

#Running ANCOVA for d_delta TAS
 ancova_model <- aov(d_delta ~ Pt.group *  tas_tot, data = ΓAED)
 summary(ancova_model)
 main_group <- emmeans(ancova_model, pairwise ~ Pt.group)
 
# Running ANCOVA for c_delta IRI
 ancova_c_delta <- aov(c_delta ~ Pt.group * (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = ΓAED)
 summary(ancova_c_delta)
 
# Running ANCOVA for c_delta TAS 
 ancova_model <- aov(c_delta ~ Pt.group * tas_tot, data = ΓAED)
 summary(ancova_model)
 
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################