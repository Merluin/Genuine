###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Emotion Intensity Judgment (EIJ)
#
#  Update:      12/11/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages

# Data loading
load("data/psychopy_dataset.RData")

# Normality Test for the entire RT distribution using Shapiro-Wilk test
shapiro_test_result <- shapiro.test(EA_dataset$EA)

# Visual inspection of RT distribution and variance using box plots
ggplot(EA_dataset, aes(x = interaction(Pt.group, File.emotion, Exp.session), y = EA)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability

# Fit the LMER model using glmer for simplicity

# Set contrasts for categorical predictors in the model
contrasts(EA_dataset$Pt.group) <- contr.sum(4)
contrasts(EA_dataset$Exp.session) <- contr.sum(3)
contrasts(EA_dataset$File.emotion) <- contr.sum(3)

# baseline difference
EA_dataset_baseline <- EA_dataset %>% 
  filter(Exp.session == "baseline")

# Model with interaction between session, group, and emotion
fit <- lmer(EA ~ Pt.group * File.emotion + (1|Pt.id),
             data = EA_dataset_baseline)

car::Anova(fit, type = "III")
plot(allEffects(fit))
main_emotion <- emmeans(fit, pairwise ~ File.emotion, adjust = "bonf")

fit <- lmer(EIJ.intensity ~ File.elicitation * File.emotion  + (1 | Pt.id),
             data = dataset)
car::Anova(fit, type = "III")

model <- aov_ez("Pt.id", "EIJ.intensity", data = dataset, within = c( "File.elicitation" , "File.emotion" ) )
            
main_emotion <- emmeans(model, pairwise ~ File.elicitation | File.emotion, adjust = "bonf")


# Fit the generalized linear mixed-effects model (lmer)
fit0 <- lmer(EA ~ 1  + (1 | Pt.id),
                data = EA_dataset)
fit1 <- lmer(EA ~ File.emotion  + (1 | Pt.id),
                data = EA_dataset)
fit2 <- lmer(EA ~ Exp.session + (1 | Pt.id),
                data = EA_dataset)
fit3 <- lmer(EA ~ Pt.group + (1 | Pt.id),
                data = EA_dataset)
fit4 <- lmer(EA ~ Exp.session * Pt.group + (1 | Pt.id),
                data = EA_dataset)
fit5 <- lmer(EA ~ File.emotion * Pt.group + (1 | Pt.id),
                data = EA_dataset)
fit6 <- lmer(EA ~ File.emotion * Exp.session * Pt.group + (1 | Pt.id),
                data = EA_dataset)

# Compare the models using ANOVA
anova(fit0, fit1, fit2, fit3, fit4, fit5, fit6)

fit <- fit1

# Plot all effects from GLMM for visualization
plot(allEffects(fit))

# ANOVA-type analysis of the GLMM using Wald chi-square tests
car::Anova(fit)
emmeans(fit, pairwise ~  File.emotion, adjust = "bonf")

# Accuracy table
mean_sd_table <- dataset %>%
  group_by(Pt.group, File.emotion) %>%
  summarise(
    mean = mean(EIJ.accuracy) * 100,
    sd = sd(EIJ.accuracy) * 100,
    .groups = 'drop'
  ) %>%
  mutate(mean_sd = paste0(round(mean, 1), " ± ", round(sd, 1))) %>%
  select(-mean, -sd) %>%
  spread(File.emotion, mean_sd) %>%
  setNames(c("Group", "Anger", "Fearful", "Happiness")) %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla() %>%
  fontsize(part = "all", size = 7) %>%
  set_header_labels(Group = "Group")

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Emotion Intensity Judgment (EIJ)
#################################################