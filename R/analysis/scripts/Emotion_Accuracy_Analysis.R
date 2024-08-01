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
         correct = emotion.accuracy) %>%
  select(subject, emotion, group,session, correct)

dataset %>%
  group_by(group, emotion) %>%
  summarise(mean = mean(correct)*100) %>%
  spread( emotion, mean)
dataset %>%
  group_by(group, emotion) %>%
  summarise(sd = sd(correct)*100) %>%
  spread( emotion, sd)

dataset %>%
  group_by(emotion) %>%
  summarise(mean = mean(correct)*100) %>%
  spread( emotion, mean)
dataset %>%
  group_by(emotion) %>%
  summarise(sd = sd(correct)*100) %>%
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
data$scorep <- dataset$correct  # Assign genuine accuracy to scorep

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


# Fit various generalized linear mixed models (GLMMs)

# Null model with only the intercept and random effect of subject
fit0 <- glmer(scorep ~ 1 + (1|subject),           # Fixed and random effects
              data = data,                        # Data frame containing variables
              family = binomial(link = "logit"),  # Binomial family with logit link function
              weights = nt)                       # Weights to account for number of trials

# Model with group as a fixed effect
fit1 <- glmer(scorep ~ group + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

# Model with session as a fixed effect
fit2 <- glmer(scorep ~ session + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

# Model with emotion as a fixed effect
fit3 <- glmer(scorep ~ emotion + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

# Model with interaction between group and session
fit4 <- glmer(scorep ~ group * session + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

# Model with interaction between group and emotion
fit5 <- glmer(scorep ~ group * emotion + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

# Model with interaction between session, group, and emotion
fit6 <- glmer(scorep ~ session * group * emotion + (1|subject),
              data = data,
              family = binomial(link = "logit"),
              weights = nt)

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
# as it has the lowest AIC value (1072.605).
BIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)
# Based on the BIC values, the best model is fit3 (emotion),
# as it has the lowest BIC value (1087.282).

# Check residual diagnostics
par(mfrow = c(2, 2))
plot(fit5)
# This plot displays Pearson residuals against fitted values
# It is used to check the model's assumptions and identify potential issues
# Points should be randomly scattered around the horizontal line at zero
# Patterns or trends in the plot may indicate problems with model fit or heteroscedasticity

# Interpretation of the plot:
# - The residuals appear to be randomly scattered, but some patterns may be observed.
# - The slight trend observed may indicate a potential issue with model fit.

# Assign the best model (fit6) to the variable fit for further analysis
fit <- fit5

#The best-fitting model was selected based on the AIC and BIC values, despite the lack of significant p-values. This model showed the lowest AIC and BIC values, indicating it provided the best fit to the data among the models tested. The Analysis of Deviance Table (Type III Wald chi-square tests) for the selected model accuracy ~ group * emotion + (1 | subject) indicated significant main effects for emotion, χ²(2) = 52.23, p < 0.001. In which accuracy related to recognition of Anger stimuli (mean = 96.8, S.D. = 6.6) are higher than accuracy for Fearful stimuli (mean = 94.2 S.D. = 8.1), z = 7.23, p < 0.001. As expected, no main effect for the Group factor was found χ²(2) = 4.29, p = 0.11. The interaction between group and emotion was found to be significant, χ²(4) = 38.89, p < 0.001. Difference was found in Anger stimuli in which Experimental group (mean = 94.9, S.D. = 8.9) was less accurate than Control and Motor groups (mean = 97.7, S.D. = 4.27; mean = 97.7, S.D. = 5.47). 

# Plot the effects of all predictors in the model
# This helps visualize the estimated effects of predictors and their interactions
plot(allEffects(fit))

p<- dataset %>%
  group_by(emotion,group) %>%
  summarise(sd = sd(correct),
            n = n(),
            se = sd / sqrt(n),
            mean = mean(correct)) %>%
  mutate(
         emotion = case_when(emotion == "anger" ~ "Anger",
                             emotion == "fear" ~ "Fearful",
                             emotion == "happiness" ~ "Happiness"),
         emotion = factor(emotion, levels = c("Anger","Fearful","Happiness")),
         group = factor(group, levels = c("Experimental","Control","M1")),
         label = paste0(group,"_",emotion)) %>%
  ggplot( aes(x = group, y = mean, fill = emotion)) +
  geom_bar(position = position_dodge(width = 0.85), stat = "identity", color = "black") +  # Bar plot with mean values
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.85), width = 0.25) +
  # facet_grid(. ~ group) +
  #coord_flip(ylim = c(0.6, 0.9)) + 
  coord_cartesian(ylim = c(0.6, 1.1)) +# Extend the y-axis limits
  labs(x = "", y = "Mean Accuracy", fill = "Emotion") +  # Labels for axes and legend
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "bottom",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add axis lines back
    axis.ticks = element_line(color = "black"),  # Add axis ticks back
    text = element_text(family = "Helvetica"),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  scale_fill_manual(values = c("Anger" = "#D9A9CF", "Fearful" = "#994A7F","Happiness" = "#7E5084")) +
  ggsignif::geom_signif(y_position = 1.08, xmin = c( 0.71), xmax = c(1.28 ), textsize = 6,
                        annotations = c("***"))  +
  ggsignif::geom_signif(y_position = 1.08, xmin = c( 1.71), xmax = c(2.28), textsize = 6,
                        annotations = c("***")) +
  ggsignif::geom_signif(y_position = 1.03, xmin = c(1), xmax = c(1.28), textsize = 6,
                        annotations = c("***")) +
  ggsignif::geom_signif(y_position = 1.03, xmin = c(2), xmax = c(2.28), textsize = 6,
                        annotations = c("***"))  +
  ggsignif::geom_signif(y_position = 1.01, xmin = c( 1.71), xmax = c(2), textsize = 6,
                        annotations = c("***")) +
  ggsignif::geom_signif(y_position = 1.01, xmin = c(  2.71), xmax = c(3 ), textsize = 6,
                        annotations = c("***")) 

ggsave("plots/AccuracyGenuineSession3.jpg", plot = p, width = 7, height = 5, units = "in", dpi = 400)


#ggsignif::geom_signif(y_position = 0.88, xmin = c( 3.75,6.75, 7.75, 8.75), xmax = c(4.25,7.25, 8.25, 9.25), textsize = 7,
#annotations = c("***","**","***","**"))
# Perform Type III Analysis of Variance (ANOVA) on the fitted model
# Type III ANOVA is used to assess the significance of each predictor after accounting for all other predictors
car::Anova(fit, type = "III")

# Calculate estimated marginal means (EMMs) and perform pairwise comparisons for the main effect of emotion
# 'adjust = "bonf"' applies Bonferroni correction for multiple comparisons
main_emotion <- emmeans(fit, pairwise ~ emotion, adjust = "bonf")

interemotion <- emmeans(fit, pairwise ~ emotion|group, adjust = "bonf")
emmeans(fit, pairwise ~ group|emotion, adjust = "bonf")


t.test(data$scorep, mu = 0.33, alternative = "greater")




# difference between group session 1
dat <- data %>% 
  filter(session == 1)
# Model with interaction between session, group, and emotion
fit <- glmer(scorep ~  emotion * group + (1|subject),
             data = dat,
             family = binomial(link = "logit"),
             weights = nt)

car::Anova(fit, type = "III")
emmeans(fit, pairwise ~ emotion, adjust = "bonf")
plot(allEffects(fit))
emmeans(fit, pairwise ~ emotion | group, adjust = "bonf")


#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################