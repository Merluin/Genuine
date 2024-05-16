###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy optimized
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages


# Data loading
dataset_concatenation("dataset")
load("data/dataset.RData") 

# Data summarization for participants
summary_participants <- dataset %>%
  filter(Trials_loop.thisRepN == 0) %>%
  group_by( participant, emotion, elicitation) %>%
  summarise(
    genuine.acc.mean = mean(gesino_kb.corr, na.rm = TRUE),
    genuine.acc.sd = sd(gesino_kb.corr, na.rm = TRUE),
    genuine.rt.mean = mean(gesino_kb.rt, na.rm = TRUE),
    genuine.rt.sd = sd(gesino_kb.rt, na.rm = TRUE),
    genuine.slider1.mean = mean(genuine_slider.response[gesino_kb.corr == 1], na.rm = TRUE),
    genuine.slider1.sd = sd(genuine_slider.response[gesino_kb.corr == 1], na.rm = TRUE),
    genuine.slider0.mean = mean(genuine_slider.response[gesino_kb.corr == 0], na.rm = TRUE),
    genuine.slider0.sd = sd(genuine_slider.response[gesino_kb.corr == 0], na.rm = TRUE),
    emotion.acc.mean = mean(accuracy, na.rm = TRUE),
    emotion.acc.sd = sd(accuracy, na.rm = TRUE),
    emotion.slider1.mean = mean(intensity_slider.response[accuracy == 1], na.rm = TRUE),
    emotion.slider1.sd = sd(intensity_slider.response[accuracy == 1], na.rm = TRUE),
    emotion.slider0.mean = mean(intensity_slider.response[accuracy == 0], na.rm = TRUE),
    emotion.slider0.sd = sd(intensity_slider.response[accuracy == 0], na.rm = TRUE)
  ) %>%
  ungroup()

outliers<-unique(identify_outliers(summary_participants,"participant","genuine.acc.mean"))
data_ouliers <- summary_participants %>%
  filter(!participant %in% outliers)



# Data summarization for emotions
summary_emotion <- data_ouliers %>%
  group_by( emotion) %>%
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
  mutate(elicitation = "Total") %>%
  mutate(across(where(is.numeric), round, digits = 2))

# Data summarization for emotion and elicitation
summary_tot <- data_ouliers %>%
  group_by( emotion, elicitation) %>%
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


# Combine summaries
final_dataset <- rbind( summary_tot[1:2,], # Combine various summaries into final dataset to have Total
                        summary_emotion[1,],
                        summary_tot[3:4,],
                        summary_emotion[2,],
                        summary_tot[5:6,],
                        summary_emotion[3,])%>%
  select(emotion, elicitation,
         genuine.acc.mean,genuine.acc.sd,
         genuine.rt.mean,genuine.rt.sd,
         genuine.slider1.mean,genuine.slider1.sd,
         genuine.slider0.mean,genuine.slider0.sd,
         emotion.acc.mean,emotion.acc.sd,
         emotion.slider1.mean,emotion.slider1.sd,
         emotion.slider0.mean,emotion.slider0.sd)

# Count participants and clips
n_pts = length(unique(dataset$subject_id))
n_clips = length(unique(dataset_pulito$file))-1

# Create and format the flextable
table <- final_dataset%>% 
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    elicitation = "Elicitation",
    genuine.acc.mean = "Mean",
    genuine.acc.sd = "Sd",
    genuine.rt.mean = "Mean",
    genuine.rt.sd = "Sd",
    emotion.acc.mean = "Mean",
    emotion.acc.sd = "Sd",
    genuine.slider1.mean = "Mean",
    genuine.slider1.sd = "Sd",
    genuine.slider0.mean = "Mean",
    genuine.slider0.sd = "Sd",
    emotion.slider1.mean = "Mean",
    emotion.slider1.sd = "Sd",
    emotion.slider0.mean = "Mean",
    emotion.slider0.sd = "Sd")) %>% 
  add_header_row(values = c("", "", 
                            rep(c("Genuine acc", "Genuine RT", "Genuine eval correct","Genuine eval error", "Emotion acc", "Emotion int correct", "Emotion int error"), each = 2))) %>% 
  merge_v(j = c(1:2)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all")%>%
  bold( part = "body", i = c(3, 6, 9), bold = TRUE)%>%
  add_footer_lines(values = paste("eliminated subject = ",outliers))

# Printing the table for preview
table

# Adjusting font size and column width
ft <- table %>% 
  fontsize(size = 8) %>%  # Reduce font size
  width(j = 1:16, width = .5)  # Adjust column widths

# Creating a Word document with the table
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = ft)
file_path <- "objects/table_optimized.docx"
print(doc, target = file_path)

# Saving on objects in RData format
save(dataset, 
     dataset_pulito,
     summary_participants,
     summary_emotion,
     summary_tot,
     file = "data/validation.RData")

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy optimized
#################################################