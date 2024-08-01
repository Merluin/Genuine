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
  select(subject, emotion, group,session,elicitation, arousal, file) 

dataset <- left_join(autoevaluation,data,by = "file") %>%
  drop_na() %>%
  filter(elicitation != "paused") %>%
  mutate(session = case_when(session == 1 ~ "baseline",
                             session == 2 ~ "T0",
                             session == 3 ~ "T20"),
         session = factor(session,levels = c("baseline","T0","T20")),
         emotion = case_when(emotion == "anger" ~ "Anger",
                             emotion == "fear" ~ "Fearful",
                             emotion == "happiness" ~ "Happiness"),
         emotion = factor(emotion)) 

data <- dataset%>%
   group_by(subject, emotion, session, group) %>%
   summarise(EA = cor(arousal,autoval),
             arousal = mean(arousal, na.rm =TRUE),
             autoval= mean(autoval, na.rm =TRUE),
             rescaled_EA = (EA+1)/2)
data %>%
  #filter(emotion == "Anger") %>%
  group_by(emotion) %>%
  summarise(Arousal = mean(arousal, na.rm =TRUE),
            autoval= mean(autoval, na.rm =TRUE))

# Normality Test for the entire RT distribution using Shapiro-Wilk test
shapiro_test_result <- shapiro.test(data$EA)

library(glmmTMB)
# Visual inspection of RT distribution and variance using box plots
ggplot(data, aes(x = interaction(group, emotion, session), y = EA)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability

# Fit the GLMER model using glmer for simplicity

# Set contrasts for categorical predictors in the model
contrasts(data$group) <- contr.sum(3)
contrasts(data$session) <- contr.sum(3)
contrasts(data$emotion) <- contr.sum(3)

# baseline difference
dat <- data %>% 
  filter(session == "baseline")
# Model with interaction between session, group, and emotion
fit <- glmer(EA ~ group * emotion + (1|subject),
             data = dat)

car::Anova(fit, type = "III")
plot(allEffects(fit))
emmeans(fit, pairwise ~ emotion, adjust = "bonf")


# Fit the generalized linear mixed-effects model (GLMM) using Gamma family
fit0 <- glmmTMB(rescaled_EA ~ 1  + (1 | subject),
               data = data,
               family = beta_family())
fit1 <- glmmTMB(rescaled_EA ~ emotion  + (1 | subject),
                data = data,
                family = beta_family())
fit2 <- glmmTMB(rescaled_EA ~ session + (1 | subject),
               data = data,
               family = beta_family())
fit3 <- glmmTMB(rescaled_EA ~ group + (1 | subject),
               data = data,
               family = beta_family())
fit4 <- glmmTMB(rescaled_EA ~ session * group + (1 | subject),
                data = data,
                family = beta_family())
fit5 <- glmmTMB(rescaled_EA ~ emotion * group + (1 | subject),
                data = data,
                family = beta_family())
fit6 <- glmmTMB(rescaled_EA ~ emotion * session * group + (1 | subject),
             data = data,
             family = beta_family())


# Compare the models using ANOVA
anova(fit0, fit1, fit2, fit3, fit4, fit5, fit6)

# Plot all effects from GLMM for visualization
plot(allEffects(fit))

fit <- fit5
# ANOVA-type analysis of the GLMM using Wald chi-square tests
car::Anova(fit)
emmeans(fit, pairwise ~  emotion, adjust = "fdr")

summary_data <- dataset %>%
  group_by(group, emotion, session) %>%
  summarise(
    R = cor(arousal, autoval),
    p_value = cor.test(arousal, autoval)$p.value
  ) %>%
  mutate(
    p_format = ifelse(p_value <= 0.001, 0.001,0.01),
    label = paste0("R = ", round(R, 2), "\np < ", p_format)
  )

ggplot(dataset, aes(x = arousal, y = autoval, color = emotion)) +
  #geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(group ~  emotion +session) +
  labs(x = "Participant evaluation of video intensity", y = "Actor auto-evaluation") +
  coord_cartesian(ylim = c(2, 9),xlim = c(0, 9)) +# Extend the y-axis limits
  theme(
    #panel.background = element_rect(fill = "white", colour = "white"),  # Set background to white
    #panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add axis lines back
    axis.ticks = element_line(color = "black"),  # Add axis ticks back
    text = element_text(family = "Helvetica"),
    # axis.text.x = element_text(angle = 45, hjust = 1) # 
  ) +
  geom_text(data = summary_data, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.1, inherit.aes = FALSE, size = 3) +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  scale_y_continuous(breaks = seq(2, 9, by = 1)) +
  scale_color_manual(values = c("Anger" = "#A9CFEF", "Fearful" = "#4A7FBB","Happiness" = "#506F8E"))

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
for (i in 1:9) {
  temp <- datacor[, c(1, i + 1)] %>% drop_na()
  plot <- generate_plot(temp, cor_table[i, "Variable"], cor_table[i, "Correlation"], cor_table[i, "Adjusted_P_Value"])
  
  plot_list[[i]] <- plot
  
  # Optionally, save each individual plot
  #ggsave(paste0("plots/plot_", cor_table[i, "Variable"], ".jpg"), plot = plot, width = 10, height = 6, units = "in", dpi = 300)
}
# Combine all plots into a single plot grid
ordered_plot_list <- list(
  plot_list[[3]],
  plot_list[[1]],
  plot_list[[2]],
  plot_list[[6]],
  plot_list[[4]],
  plot_list[[5]],
  plot_list[[9]],
  plot_list[[7]],
  plot_list[[8]]
)
combined_plot <- cowplot::plot_grid(plotlist = ordered_plot_list, ncol = 3)

# Save combined plot
ggsave("plots/plot2.jpg", plot = combined_plot, width = 10, height = 8, units = "in", dpi = 300)
#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################