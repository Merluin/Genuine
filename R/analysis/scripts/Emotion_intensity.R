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

dataset <- read_excel("data/dati_summary_lf.xlsx", sheet = "Sheet 1")  %>%
  filter(
    subject != 8, subject != 11, subject != 19, 
    subject != 30) %>%
  mutate(subject = as.factor(subject),
         emotion = as.factor(emotion),
         group = as.factor(group),
         arousal = emotion.slider.correct) %>%
  select(subject, emotion, group,session, arousal)

dataset %>%
  group_by(group, emotion) %>%
  summarise(mean = mean(arousal)) %>%
  spread( emotion, mean)
dataset %>%
  group_by(group, emotion) %>%
  summarise(sd = sd(arousal)) %>%
  spread( emotion, sd)

# Check if filters are correctly applied
unique(dataset$group)  # Check unique group values
unique(dataset$subject)  # Check unique subject values
unique(dataset$session)  # Check unique session values

# model selection
data <- dataset # Avoid overwriting existing object

# Data preparation for GLM (Generalized Linear Model)

# Add a column for the number of trials
data$nt <- 48  # Number of trials

# Create an easy-to-use code for genuine accuracy
data$scorep <- dataset$arousal  # Assign genuine accuracy to scorep

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

# # Define a custom contrast matrix for 3 levels
# custom_contrasts <- matrix(c(
#   2, -1, -1,  # Contrast 1: Pre-treatment vs. average of both post-treatments
#   -1,  1,  0,  # Contrast 2: Post-treatment1 vs. Pre-treatment
#   -1,  0,  1   # Contrast 3: Post-treatment2 vs. Pre-treatment
# ), byrow = TRUE, nrow = 3)
# # Name the rows and columns for clarity
# rownames(custom_contrasts) <- c("Pre vs Post Avg", "Post1 vs Pre", "Post2 vs Pre")
# colnames(custom_contrasts) <- levels(data$session)
# # Assign the custom contrasts to the factor
# contrasts(data$session) <- custom_contrasts

# Model with interaction between session, group, and emotion
fit0 <- glm(scorep ~ 1,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity"))  
fit1 <- glm(scorep ~ session,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity"))  
fit2 <- glm(scorep ~ group,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity"))  
fit3 <- glm(scorep ~ emotion,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity"))  
fit4 <- glm(scorep ~ group * emotion,
             data = data,                      # Data frame containing variables
             family = gaussian(link = "identity"))  
fit5 <- glm(scorep ~ group * session,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity")) 
fit6 <- glm(scorep ~ session * group * emotion,
           data = data,                      # Data frame containing variables
           family = gaussian(link = "identity")) 

# Compare the models using ANOVA
anova(fit0, fit1, fit2, fit3, fit4, fit5, fit6)

AIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)
BIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)

# Check Variance Inflation Factor (VIF) for multicollinearity
vif(fit)
# The VIF values for the model are all very close to 1, 
# which indicates that there is no significant multicollinearity among the predictors.

# Check residual diagnostics
par(mfrow = c(2, 2))
plot(fit)


# Plot the effects of all predictors in the model
# This helps visualize the estimated effects of predictors and their interactions
plot(allEffects(fit3))

# Perform Type III Analysis of Variance (ANOVA) on the fitted model
# Type III ANOVA is used to assess the significance of each predictor after accounting for all other predictors
car::Anova(fit3, type = "III")

# Calculate estimated marginal means (EMMs) and perform pairwise comparisons for the main effect of emotion
# 'adjust = "bonf"' applies Bonferroni correction for multiple comparisons
main_emotion <- emmeans(fit3, pairwise ~ emotion, adjust = "bonf")




#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################

# Install and load necessary packages for ordinal analysis
install.packages("MASS")
install.packages("car")
install.packages("lmtest")

library(MASS)
library(car)
library(lmtest)

# Prepare your data
data$intensity <- factor(data$intensity, levels = 0:9, ordered = TRUE)

# Fit multiple ordinal logistic regression models
fit0 <- polr(intensity ~ 1, data = data, Hess = TRUE)
fit1 <- polr(intensity ~ group, data = data, Hess = TRUE)
fit2 <- polr(intensity ~ session, data = data, Hess = TRUE)
fit3 <- polr(intensity ~ emotion, data = data, Hess = TRUE)
fit4 <- polr(intensity ~ group * session, data = data, Hess = TRUE)
fit5 <- polr(intensity ~ group * emotion, data = data, Hess = TRUE)
fit6 <- polr(intensity ~ session * group * emotion, data = data, Hess = TRUE)

# Compare models using AIC and BIC
aic_values <- data.frame(
  Model = c("fit0", "fit1", "fit2", "fit3", "fit4", "fit5", "fit6"),
  AIC = c(AIC(fit0), AIC(fit1), AIC(fit2), AIC(fit3), AIC(fit4), AIC(fit5), AIC(fit6)),
  BIC = c(BIC(fit0), BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4), BIC(fit5), BIC(fit6))
)

print(aic_values)

# Perform likelihood ratio tests for nested models
anova(fit5, fit6, test="Chisq")
anova(fit4, fit6, test="Chisq")
anova(fit3, fit6, test="Chisq")

# Check for multicollinearity
vif_values <- vif(fit6)
print(vif_values)

# Examine model diagnostics
par(mfrow = c(2, 2))
plot(fit6)

# Re-fit a simpler model without three-way interactions
fit_simple <- polr(intensity ~ session + group + emotion + session:group + session:emotion + group:emotion, data = data, Hess = TRUE)
summary(fit_simple)

# Check Anova for the simpler model
anova_results <- car::Anova(fit_simple, type = "III")
print(anova_results)

# Compare AIC of fit_simple and fit6
aic_fit6 <- AIC(fit6)
aic_fit_simple <- AIC(fit_simple)

print(c(fit6 = aic_fit6, fit_simple = aic_fit_simple))
