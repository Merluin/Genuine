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
dataset %>%
  summarise(accuracy = mean(EIJ.accuracy,na.rm = TRUE),
            sd = sd(EIJ.accuracy,na.rm = TRUE)) 


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




contagione <- sdt_data %>% 
  select(Pt.id, File.emotion, Exp.session, Pt.group, HR ,   FAR) %>%
  gather(rate, score, c(5:6))
anova_contagion <- aov_ez("Pt.id", "score", contagione, within = c("File.emotion","rate","Exp.session"), between = "Pt.group")
interaction <- emmeans(anova_contagion, pairwise ~ Exp.session|rate|Pt.group, adjust = "bonferroni")



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
maind_emotion <- emmeans(anova_Md, pairwise ~ File.emotion, adjust = "bonferroni")
plot(maind_emotion)

# Perform ANOVA for response bias (meta c) at baseline
anova_Mc_baseline <- aov_ez("Pt.id", "c", meta_dat_session1, within = "File.emotion", between = "Pt.group")

# Perform ANOVA for response bias (meta c) for full model
anova_Mc <- aov_ez("Pt.id", "c", meta_sdt_data, within = c("File.emotion","Exp.session"), between = "Pt.group")
mainc_emotion <- emmeans(anova_Mc, pairwise ~ File.emotion, adjust = "bonferroni")
plot(mainc_emotion)




# Calculate mean d' and c for each emotion, session, and group
data_genuine_mean <-sdt_data %>%
  group_by(File.emotion, Exp.session, Pt.group) %>%
  summarise(d_prime = mean(d_prime, na.rm = TRUE),
            c = mean(c, na.rm = TRUE)) %>%
  ungroup() 

x_breaks <- c("baseline", "T0", "T20")

# Plot for sensitivity (d')
plot_data <- sdt_data %>%
  group_by(Pt.id, Exp.session,Pt.group) %>% 
  summarise(
    mean = mean(d_prime, na.rm = TRUE),
    mean_c = mean(c, na.rm = TRUE),
    .groups = "drop")


# Install necessary packages if not installed
if (!require("viridis")) install.packages("viridis")
if (!require("ggsci")) install.packages("ggsci")

# Use the viridis color scale for color-blind-friendly colors
plot_d <- plot_data %>%
  mutate(Pt.group = case_when(
    Pt.group == "Exp1rpSTS-rIFG" ~ "pSTS-IFG",
    Pt.group == "Ctrl1rIFG-rpSTS" ~ "IFG-pSTS",
    Pt.group == "Exp2rIFG-rM1" ~ "IFG-M1",
    Pt.group == "Ctrl2rM1-rIFG" ~ "M1-IFG"
  )) %>%
  ggplot(aes(x = Exp.session, y = mean, fill = Pt.group, color = Pt.group)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(position = position_jitter(width = .05), size = 2, alpha = .5) +
  geom_boxplot(width = .3, outlier.shape = NA, alpha = 0, notch = FALSE) +  # Removed `guides = FALSE`
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  facet_wrap(. ~ Pt.group, ncol = 2) +
  scale_fill_viridis_d(option = "D") +  # Viridis palette for fill
  # scale_color_viridis_d(option = "D") +  # Removed since it conflicts with manual coloring
  scale_color_manual(values = rep("gray20", length(unique(plot_data$Pt.group)))) +  # Dark gray for color
  theme_minimal() +
  scale_y_continuous(limits = c(0, 3)) +  # Better than `ylim(0, 3)` because it keeps all data points
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.2, "cm")
  ) +
  labs(x = "", y = "Sensitivity (dprime)")


# Add the significance annotations as before
plot_d <- plot_d +
  ggsignif::geom_signif(
    comparisons = list(c("1", "2")),
    data = filter(plot_data, Pt.group == "IFG-M1"),
    map_signif_level = FALSE,
    y_position = 2.19,
    textsize = 4, size = 0.4, color = "black",
    annotations = "**"
  ) +
  ggsignif::geom_signif(
    comparisons = list(c("1", "3")),
    data = filter(plot_data, Pt.group == "IFG-M1"),
    map_signif_level = FALSE,
    y_position = 2.37,
    textsize = 4, size = 0.4, color = "black",
    annotations = "***"
  ) +
  ggsignif::geom_signif(
    comparisons = list(c("1", "2")),
    data = filter(plot_data, Pt.group == "pSTS-IFG"),
    map_signif_level = FALSE,
    y_position = 2.21,
    textsize = 4, size = 0.4, color = "black",
    annotations = "*"
  ) +
  ggsignif::geom_signif(
    comparisons = list(c("1", "3")),
    data = filter(plot_data, Pt.group == "pSTS-IFG"),
    map_signif_level = FALSE,
    y_position = 2.39,
    textsize = 4, size = 0.4, color = "black",
    annotations = "+"
  )


# Display the plot
print(plot_d)
ggsave("Figure3.png", plot = plot_d, 
       width = 14.41, height = 12.15, units = "cm", 
       dpi = 1200)


# plot_d <- as.data.frame(interaction_d) %>%
#   filter(Exp.session != ".") %>%
#   ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
#   geom_point(position = position_dodge(width = 0.3), size = 3, color = "black") +
#   geom_line(position = position_dodge(width = 0.3), size = 0.5, color = "black") +
#   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
#                 width = 0.2, position = position_dodge(width = 0.3), color = "black") +
#   theme_minimal() +
#   coord_fixed(ylim = c(0.5, 2.5)) +
#   labs(x = "", y = "Sensitivity (dprime)") +
#   theme(
#     legend.position = "none", # Remove legend
#     panel.grid = element_blank(), # Remove grid lines
#     axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
#     axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
#     axis.ticks.length = unit(0.2, "cm") # Set tick length
#   )+ theme(
#     plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
#   )+
#   scale_x_discrete(breaks = x_breaks)  +
#   ggsignif::geom_signif(
#     xmin = 1 + 0.1, xmax = 2 + 0.1, y_position = 1.75, textsize = 4, 
#     size = 0.4, # Make the bar more subtle by reducing thickness
#     color = "black", annotations = c("**"), tip_length = 0, 
#     position = position_dodge(width = 0.1)) +
#   ggsignif::geom_signif(
#     xmin = 1 + 0.1, xmax = 3 + 0.1, y_position = 2.05, textsize = 4, 
#     size = 0.4, # Make the bar more subtle
#     color = "black", annotations = c("***"), tip_length = 0, 
#     position = position_dodge(width = 0.1)) +
#   ggsignif::geom_signif(
#     xmin = 1 + 0.03, xmax = 2 + 0.03, y_position = 1.6, textsize = 4, 
#     size = 0.4, # Subtle bar
#     color = "black", annotations = c("*"), tip_length = 0, # Use superscript
#     position = position_dodge(width = 0.1)) +
#   ggsignif::geom_signif(
#     xmin = 1 + 0.03, xmax = 3 + 0.03, y_position = 1.9, textsize = 4, 
#     size = 0.4, # Subtle bar
#     color = "black", annotations = c("+"), tip_length = 0, 
#     position = position_dodge(width = 0.1))


# # Extract the legend from the plot
# plot <- as.data.frame(interaction_d) %>%
#   filter(Exp.session != ".") %>%
#   ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
#   geom_point(position = position_dodge(width = 0.3), size = 3, color = "black")+ 
#   guides(shape = guide_legend(nrow = 1))
# legend <- cowplot::get_legend(plot)
# 
# # Plot for response bias (c)
#  plot_c <- as.data.frame(interaction_c) %>%
#    filter(Exp.session != ".") %>%
#    ggplot(aes(x = Exp.session, y = emmean, group = Pt.group, shape = Pt.group)) +
#    geom_point(position = position_dodge(width = 0.3), size = 3, color = "black") +
#    geom_line(position = position_dodge(width = 0.3), size = 0.5, color = "black") +
#    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
#                  width = 0.2, position = position_dodge(width = 0.3), color = "black") +
#    theme_minimal() +
#    coord_fixed(ylim = c(-0.4, 0.4)) +
#    labs(x = "", y = "Bias (criterion)") +
#    theme(
#      legend.position = "none", # Remove legend
#      panel.grid = element_blank(), # Remove grid lines
#      axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
#      axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
#      axis.ticks.length = unit(0.2, "cm") # Set tick length
#    ) 
#    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Adjust margins to be consistent
#  
plot_c <- plot_data %>%
  ggplot(aes(x = Exp.session, y = mean_c, fill = Pt.group)) +
  #geom_rain(alpha = 0.7) +
  geom_flat_violin(position = position_nudge(x = .2, y =0), alpha = .8)+
  geom_point(aes(y = , color = Pt.group), 
             position = position_jitter(width = .05), size = 2, alpha = .5) +
  geom_boxplot(width = .3, guides = FALSE, outlier.shape = NA, alpha = 0, notch = FALSE) +
  stat_summary(fun= mean, geom = "point", shape = 21, size = 3, fill = "black") +
  facet_grid(. ~ Pt.group) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    panel.grid = element_blank(), # Remove grid lines
    axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
    axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
    axis.ticks.length = unit(0.2, "cm") # Set tick length
  )+
    labs(x = "", y = "Bias (criterion)")
  

 # Combine the two plots in one row
 plots_row <- cowplot::plot_grid(
   plot_d, plot_c, 
   ncol = 1, 
   rel_widths = c(1, 1), # Ensure equal widths for both plots
   align = "vh" # Align vertically and horizontally to make them the same size
 )
 
 
 # # Combine the plots row with the legend
 # final_plot <- cowplot::plot_grid(
 #   plots_row, # Add the row of plots
 #  legend, # Add the legend below
 #   ncol = 1, # Combine vertically
 #   rel_heights = c(1, 0.1) # Adjust relative heights (legend closer to the plots)
 # )
 # 
 # # Print the final plot
 # print(final_plot)
 
 # Running ANCOVA for d_delta IRI
Exp1 <- ΓAED %>%
   filter(Pt.group == "Exp1rpSTS-rIFG") 
 
Exp2 <- ΓAED %>%
  filter(Pt.group ==  "Exp2rIFG-rM1") 

 ancova_d_delta <- aov(d_delta ~  (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = Exp1)
 summary(ancova_d_delta)
 ancova_d_delta <- aov(d_delta ~  (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = Exp2)
 summary(ancova_d_delta)

#Running ANCOVA for d_delta TAS
 ancova_model <- aov(d_delta ~   tas_tot, data = Exp1)
 summary(ancova_model)
 ancova_model <- aov(d_delta ~   tas_tot, data = Exp2)
 summary(ancova_model)

# Running ANCOVA for c_delta IRI
 ancova_c_delta <- aov(c_delta ~ (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = Exp1)
 summary(ancova_c_delta)
 ancova_c_delta <- aov(c_delta ~ (fantasy + perspective_taking + empathic_concern + personal_distress + iri_tot), 
                       data = Exp2)
 summary(ancova_c_delta)
 
# Running ANCOVA for c_delta TAS 
 ancova_model <- aov(c_delta ~  tas_tot, data = Exp1)
 summary(ancova_model)
 ancova_model <- aov(c_delta ~  tas_tot, data = Exp2)
 summary(ancova_model)
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot signal detection
#################################################