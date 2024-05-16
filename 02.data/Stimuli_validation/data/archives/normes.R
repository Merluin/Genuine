###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: PEDFE normative data
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages


# Data loading
data <- read_excel("data/norme.xlsx", sheet = "norme")
dataset_concatenation("dataset")
load("data/dataset.RData") 

data <- data %>%
  filter(PEDFE_code != "2_ps_5")

summary_participants <- dataset %>%
  filter(Trials_loop.thisRepN == 0) %>%
  group_by( participant, emotion, elicitation, file) %>%
  summarise(
    PEDFE_code = sub("\\.mp4$", "", file),
    genuine.acc.mean = mean(gesino_kb.corr, na.rm = TRUE),
    genuine.rt.mean = mean(gesino_kb.rt, na.rm = TRUE),
    genuine.slider1.mean = mean(genuine_slider.response[gesino_kb.corr == 1], na.rm = TRUE),
    genuine.slider0.mean = mean(genuine_slider.response[gesino_kb.corr == 0], na.rm = TRUE),
    emotion.acc.mean = mean(accuracy, na.rm = TRUE),
    emotion.slider1.mean = mean(intensity_slider.response[accuracy == 1], na.rm = TRUE),
    emotion.slider0.mean = mean(intensity_slider.response[accuracy == 0], na.rm = TRUE),
  ) %>%
  ungroup()

summary_tot <- summary_participants %>%
  group_by( emotion, elicitation, PEDFE_code)  %>%
  summarise(
    genuine.acc.sd = sd(genuine.acc.mean, na.rm = TRUE),
    genuine.acc.mean = mean(genuine.acc.mean, na.rm = TRUE),
    genuine.rt.sd = sd(genuine.rt.mean, na.rm = TRUE),
    genuine.rt.mean = mean(genuine.rt.mean, na.rm = TRUE),
    genuine.slider1.sd = sd(genuine.slider1.mean, na.rm = TRUE),
    genuine.slider1.mean = mean(genuine.slider1.mean, na.rm = TRUE),
    genuine.slider0.sd = sd(genuine.slider0.mean, na.rm = TRUE),
    genuine.slider0.mean = mean(genuine.slider0.mean, na.rm = TRUE),
    emotion.acc.sd = sd(emotion.acc.mean, na.rm = TRUE),
    emotion.acc.mean = mean(emotion.acc.mean, na.rm = TRUE),
    emotion.slider1.sd = sd(emotion.slider1.mean, na.rm = TRUE),
    emotion.slider1.mean = mean(emotion.slider1.mean, na.rm = TRUE),
    emotion.slider0.sd = sd(emotion.slider0.mean, na.rm = TRUE),
    emotion.slider0.mean = mean(emotion.slider0.mean, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), round, digits = 2))

summary_tot <- left_join(summary_tot, data, by = "PEDFE_code") %>%
  mutate(genuine.acc.mean = genuine.acc.mean*100,
         emotion.acc.mean = emotion.acc.mean*100)

summary <- summary_tot %>%
  group_by(elicitation, emotion) %>%
  summarise(genuine.acc.mean = mean(genuine.acc.mean, na.rm = TRUE),
            genuine.acc.face = mean(Genuine.acc.face, na.rm = TRUE),
            genuine.acc.full = mean(Genuine.acc.full, na.rm = TRUE),
            emotion.acc.mean = mean(emotion.acc.mean, na.rm = TRUE),
            emotion.acc.face = mean(Emotion.acc.face, na.rm = TRUE),
            emotion.acc.full = mean(Emotion.acc.full, na.rm = TRUE))

test_genuine <- summary_tot %>%
  select(elicitation, emotion,PEDFE_code,
         genuine.acc.mean,
         Genuine.acc.face,
         Genuine.acc.full ) %>%
  'colnames<-'(c("elicitation","emotion", "file","short","solo","full")) %>%
  gather( dataset, acc, c(4:6))

test_emotion <- summary_tot %>%
  select(elicitation, emotion, PEDFE_code,
         emotion.acc.mean,
         Emotion.acc.face,
         Emotion.acc.full )%>%
  'colnames<-'(c("elicitation","emotion", "file","short","solo","full"))%>%
  gather( dataset, acc, c(4:6))


  

# Anova test
test_emotion$acclog <- log(test_emotion$acc)
lm_model <- lm(acclog ~ elicitation * emotion * dataset, data = test_emotion)
summary(lm_model)
flexplot::visualize(lm_model, plot = "residuals")

test_genuine$acclog <- log(test_genuine$acc)
lm_model <- lm(acclog ~ elicitation * emotion * dataset, data = test_genuine)
summary(lm_model)
flexplot::visualize(lm_model, plot = "residuals")

Anova(lm_model, type = "III")

genuine <- aov(acc ~ elicitation * emotion * dataset, data = test_genuine)
summary(genuine)
posthoc_dataset <- emmeans(genuine, pairwise ~ dataset|emotion|elicitation)
#anger genuine short < full/solo
#happy paused short < full/solo


emotion <- aov(acc ~ elicitation * emotion * dataset, data = test_emotion)
summary(emotion)
inter_emotion_dataset <- emmeans(emotion, pairwise ~ dataset|emotion)
#anger short > full/solo
#fear short > full/solo

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - PEDFE normative data
#################################################