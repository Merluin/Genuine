###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
# Description: Generate an exploratory plot to visualize the accuracy of each subject for each video
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages

# Data loading
load("data/dataset.RData") 

# Prepare data
summary_video <- dataset %>%
  group_by( file, emotion, elicitation, session, group) %>%
  summarise(genuine.acc.mean = mean(gesino_kb.corr, na.rm = TRUE))

# plots
genuine_pts_plot <- summary_video %>%
  ggplot(aes(x = session, y = genuine.acc.mean, group = file, color = file)) +
  geom_point() +
  geom_line() +
  geom_text(data = summary_video%>%filter(session == 1),aes(x = 2, y = genuine.acc.mean,group = file, color = file,label = file),position = position_dodge(width = 2)) +
  facet_grid(emotion*elicitation ~ group) +
  theme_minimal() + theme(legend.position="none")
  
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - exploratory plot
#################################################