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
load("data/validation.RData") 

data <- dataset_pulito %>%
  filter(Trials_loop.thisRepN == 0) %>%
  mutate(subject = as.factor(participant),
         emotion = as.factor(emotion),
         elicitation = as.factor(elicitation),
         correct = accuracy) %>%
  select(subject, emotion, elicitation, correct)

# Data Exploration (Optional but recommended)
# Summary statistics, structure, and basic visualizations
print(head(data))
print(summary(data))
# Assuming 'data' is your dataframe and 'correct' should be a factor with levels indicating correct or incorrect responses.
data$correct <- factor(data$correct, levels = c(0, 1), labels = c("Incorrect", "Correct"))

# Now let's plot again
ggplot(data, aes(x = emotion, fill = correct)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Incorrect" = "red", "Correct" = "green")) +
  labs(fill = "Response Correctness", y = "Proportion", x = "Emotion") +
  theme_minimal()

  
# Model Fitting
fit0 <- glmer(correct ~ 1 + (1 | subject), data = data, family = binomial)
fit1 <- glmer(correct ~ emotion + (1 | subject), data = data, family = binomial)
fit2 <- glmer(correct ~ emotion + elicitation + (1 | subject), data = data, family = binomial)
fit3 <- glmer(correct ~ emotion * elicitation + (1 | subject), data = data, family = binomial)

# Model Comparison
anova(fit0, fit1, fit2, fit3)

# Model Summary
print(summary(fit2))

# Effects Plot
plot(allEffects(fit2))

# Model Diagnostics (Checking for model fit and assumptions)
# Example: Plotting residuals
plot(residuals(fit2))

# anova
AnovaFit2 <- car::Anova(fit2, type = 3) 

#Contrasts fit2
m1<-emmeans(fit2, pairwise ~ emotion)
m2<-emmeans(fit2, pairwise ~ elicitation)



#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy analysis emotion
#################################################