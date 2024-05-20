###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        05/03/2024
#  Description: GLM ACCURACY GENUINE
#
#  Update:      05/03/2024
###########################################################################

# Clearing workspace
rm(list = ls())  # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all()  # Load necessary functions and packages

# Data loading and eliminate outliers and 5 of the 6 M1 group
dataset <- read_excel("data/accuracy_genuine_lf.xlsx", sheet = "Sheet 1") %>% 
  filter(session != 2, 
         subject != 8, subject != 11, subject != 19, 
         subject != 30, subject != 31, subject != 32, 
         subject != 33, subject != 34, subject != 35)

# Compute inverse efficiency score and update group labels
dataset <- dataset %>%
  mutate(ies = genuine.rt / genuine.accuracy,  # Compute inverse efficiency score
         group = ifelse(group == "Experimental", group, "ctrl"))  # Mutate group to be "Experimental" or "ctrl"

# Check if filters are correctly applied
unique(dataset$group)  # Check unique group values
unique(dataset$subject)  # Check unique subject values
unique(dataset$session)  # Check unique session values

# model selection
data <- dataset # Avoid overwriting existing object

# Data preparation for GLM (Generalized Linear Model)

# Add a column for the number of trials
data$nt <- 48  # Number of trials

# Create an easy-to-use code for genuine ies
data$scorep <- dataset$ies  # Assign ies to scorep

# Convert variables to appropriate data types and set factors
data <- data %>%
  mutate(scorep = as.numeric(scorep),  # Convert scorep to numeric
         group = as.factor(group),  # Convert group to factor
         session = as.factor(session),  # Convert session to factor
         emotion = as.factor(emotion))  # Convert emotion to factor

# Set contrasts for factors to use sum coding
contrasts(data$group) <- contr.sum(length(unique(data$group)))  # Sum coding for group
contrasts(data$emotion) <- contr.sum(length(unique(data$emotion)))  # Sum coding for emotion
contrasts(data$session) <- contr.sum(length(unique(data$session)))  # Sum coding for session


# Fit variousm linear mixed models (GLMMs)

# Null model with only the intercept and random effect of subject
fit0 <- lmer(scorep ~ 1 + (1|subject),           # Fixed and random effects
              data = data)                       # Weights to account for number of trials

# Model with group as a fixed effect
fit1 <- glmer(scorep ~ group + (1|subject),
              data = data)

# Model with session as a fixed effect
fit2 <- glmer(scorep ~ session + (1|subject),
              data = data)

# Model with emotion as a fixed effect
fit3 <- glmer(scorep ~ emotion + (1|subject),
              data = data)

# Model with interaction between group and session
fit4 <- glmer(scorep ~ group * session + (1|subject),
              data = data)

# Model with interaction between group and emotion
fit5 <- glmer(scorep ~ group * emotion + (1|subject),
              data = data)

# Model with interaction between session, group, and emotion
fit6 <- glmer(scorep ~  group * emotion * session + (1|subject),
              data = data)

# Compare the models using ANOVA
anova(fit0, fit1, fit2, fit3, fit4, fit5, fit6)

# Check Variance Inflation Factor (VIF) for multicollinearity
vif(fit6)
# The VIF values for the model are all very close to 1, 
# which indicates that there is no significant multicollinearity among the predictors.

# Check AIC and BIC for model comparison
# AIC (Akaike Information Criterion) is a measure of the relative quality of statistical models for a given dataset.
# It estimates the goodness of fit and includes a penalty for the number of parameters to avoid overfitting.
# The model with the lowest AIC value is considered the best model among the compared models.
# BIC (Bayesian Information Criterion) is similar to AIC but includes a stronger penalty for models with more parameters.
# It is used to select the best model by balancing goodness of fit with model complexity.
# The model with the lowest BIC value is considered the best model among the compared models.

AIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)
# Based on the AIC values, the best model is fit6 (session * group * emotion),
# as it has the lowest AIC value (1092.109).
BIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)
# Based on the BIC values, the best model is fit3 (emotion),
# as it has the lowest BIC value (1105.695).

# Check residual diagnostics
par(mfrow = c(2, 2))
plot(fit6)
# This plot displays Pearson residuals against fitted values
# It is used to check the model's assumptions and identify potential issues
# Points should be randomly scattered around the horizontal line at zero
# Patterns or trends in the plot may indicate problems with model fit or heteroscedasticity

# Interpretation of the plot:
# - The residuals appear to be randomly scattered, but some patterns may be observed.
# - The slight trend observed may indicate a potential issue with model fit.

# Assign the best model (fit6) to the variable fit for further analysis
fit <- fit6

# Plot the effects of all predictors in the model
# This helps visualize the estimated effects of predictors and their interactions
plot(allEffects(fit))

# Perform Type III Analysis of Variance (ANOVA) on the fitted model
# Type III ANOVA is used to assess the significance of each predictor after accounting for all other predictors
car::Anova(fit, type = "III")

# Calculate estimated marginal means (EMMs) and perform pairwise comparisons for the main effect of emotion
# 'adjust = "bonf"' applies Bonferroni correction for multiple comparisons
main_session <- emmeans(fit, pairwise ~ session, adjust = "bonf")



#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - GLM ACCURACY GENUINE
#################################################