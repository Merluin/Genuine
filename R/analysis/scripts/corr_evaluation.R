###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy analysis emotion
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages


# Data loading
autoevaluation <- read_excel("data/autovalutazione_clips.xlsx") %>%
  'colnames<-'(c("Emotion", "Elicitation", "file","autoval"))

load("data/dataset.RData")

data <- dataset %>%
  select(participant, emotion, elicitation, file, group, session, intensity_slider.response) %>%
  filter(
    participant != 8, participant != 11, participant != 19, 
    participant != 30) %>%
  mutate( subject = as.factor(participant),
         emotion = as.factor(emotion),
         elicitation = as.factor(elicitation),
         group = as.factor(group),
         arousal = intensity_slider.response) %>%
  select(subject, emotion, group,session,elicitation, arousal, file) %>%
  group_by(file, session) %>%
  summarise(arousal = mean(arousal)) 

data <- left_join(autoevaluation,data,by = "file") %>%
  drop_na() 

# data selection
datacor <- data %>%
  mutate(var = paste0(Emotion,"_T",session-1 )) %>%
  spread(var, arousal) %>%
  filter(Elicitation != "paused") %>%
  select(-c(Emotion, Elicitation, file, session))

# Initialize vectors to store results
correlations <- numeric()
p_values <- numeric()

# Calculate correlations and p-values for each column with 'Actor'
for (i in 2:ncol(datacor)) {
  temp <- datacor[, c(1, i)] %>% drop_na()
  cor_results <- rcorr(as.matrix(temp), type = "pearson")
  
  correlations <- c(correlations, cor_results$r[1, 2])
  p_values <- c(p_values, cor_results$P[1, 2])
}

# Adjust p-values using FDR
adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Create a summary table
cor_table <- data.frame(
  Variable = colnames(datacor)[2:ncol(datacor)],
  Correlation = round(correlations,2),
  P_Value = round(p_values,3),
  Adjusted_P_Value = round(adjusted_p_values,3)
)

# Print the summary table
print(cor_table)


# Function to generate plots
generate_plot <- function(data, time_point, correlation, p_value) {
  ggplot(data, aes(y = autoval, x = .data[[time_point]])) +
    geom_smooth(method = "lm", se = TRUE, color = "#4A70B0", fill = "#97A9C3") +   
    geom_point(color = "#4B607D") +
    labs(title = "",
         y = "Actor evaluation",
         x = gsub("_", " ", time_point)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      text = element_text(family = "Helvetica")
    ) +
    coord_cartesian(ylim = c(1, 9),
                    xlim = c(1, 9)) +
    theme(plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm")) +
    annotate("text", x = 8, y = 5, hjust = 1.1, vjust = 1.5, size = 3,
             label = paste0("R = ", round(correlation, 2), ",\n p = ", round(p_value, 4)))
}


# Generate plots
# Initialize a list to store the plots
plot_list <- list()
# Loop to generate and store plots in the list
for (i in 1:nrow(cor_table)) {
  temp <- datacor[, c(1, i + 1)] %>% drop_na()
  plot <- generate_plot(temp, cor_table[i, "Variable"], cor_table[i, "Correlation"], cor_table[i, "Adjusted_P_Value"])
  
  plot_list[[i]] <- plot
  
  # Optionally, save each individual plot
  #ggsave(paste0("plots/plot_", cor_table[i, "Variable"], ".jpg"), plot = plot, width = 10, height = 6, units = "in", dpi = 300)
}
# Combine all plots into a single plot grid
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3)

# Save combined plot
ggsave("plots/plot2.jpg", plot = combined_plot, width = 10, height = 3, units = "in", dpi = 300)
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################