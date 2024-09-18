###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Emotion Intensity Judgment (EIJ)
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages


# Data loading
load("data/validation_dataset.RData")

# Normality Test for the entire RT distribution using Shapiro-Wilk test
shapiro_test_result <- shapiro.test(data$EA)

# Visual inspection of RT distribution and variance using box plots
ggplot(data, aes(x = interaction(emotion), y = EA)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve label readability

# Fit the GLMER model using glmer for simplicity

# Set contrasts for categorical predictors in the model
contrasts(data$emotion) <- contr.sum(3)

# Fit the generalized linear mixed-effects model (GLMM) using Gamma family
fit0 <- glmmTMB(rescaled_EA ~ 1  + (1 | subject),
                data = data,
                family = beta_family())
fit1 <- glmmTMB(rescaled_EA ~ emotion  + (1 | subject),
                data = data,
                family = beta_family())



# Compare the models using ANOVA
anova(fit0, fit1)


fit <- fit1
# ANOVA-type analysis of the GLMM using Wald chi-square tests
car::Anova(fit)
emmeans(fit, pairwise ~  emotion, adjust = "fdr")


#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Emotion Intensity Judgment (EIJ)
#################################################