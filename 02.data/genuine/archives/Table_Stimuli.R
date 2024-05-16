###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy
#
#  Update:      23/01/2024
###########################################################################

# Clear the existing workspace to avoid conflicts
rm(list=ls()) 

# Load necessary functions and packages
devtools::load_all()

# loading data ------------------------------------------------------------
dataset_concatenation("dataset")
load("data/dataset.RData") 

summary_participants <- dataset %>%
  filter(Trials_loop.thisRepN == 0) %>%
  group_by( participant, emotion, elicitation, file) %>%
  summarise(
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
  group_by( emotion, elicitation, file)  %>%
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

summary_tot <- summary_tot %>%
  select(emotion, elicitation, file,
         genuine.acc.mean,genuine.acc.sd,
         genuine.rt.mean,genuine.rt.sd,
         genuine.slider1.mean,genuine.slider1.sd,
         genuine.slider0.mean,genuine.slider0.sd,
         emotion.acc.mean,emotion.acc.sd,
         emotion.slider1.mean,emotion.slider1.sd,
         emotion.slider0.mean,emotion.slider0.sd)

n = length(unique(dataset$subject_id))

summary_tot <- summary_tot %>%
  group_by(emotion, elicitation) %>%
  mutate(min_genuine_acc_mean = min(genuine.acc.mean, na.rm = TRUE)) %>%
  ungroup() 

# Step 2: Create a marker for the minimum values
summary_tot <- summary_tot %>%
  mutate(min_marker = ifelse(genuine.acc.mean == min_genuine_acc_mean, TRUE, FALSE))

clip_list <- summary_tot$file[which(summary_tot$min_marker == TRUE)]
  
# Count participants and clips
n_pts = length(unique(dataset$subject_id))
n_clips = length(unique(dataset$file))-1

table <- summary_tot%>% 
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 9)%>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    elicitation = "Elicitation",
    file = "Mp4",
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
  add_header_row(values = c(paste("Pts =",n_pts, "mp4 = ",n_clips ), "", "File",
                            rep(c("Genuine acc", "Genuine RT", "Genuine eval correct","Genuine eval error", "Emotion acc", "Emotion int correct", "Emotion int error"), each = 2),"min_marker","min_genuine_acc_mean")) %>%  
  merge_v(j = c(1:3)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all") %>% 
  fontsize(size = 8) 

table <- delete_columns(table, j = c("min_marker","min_genuine_acc_mean"))


condition_index <- which(summary_tot[[ncol(summary_tot)]])
table <- color(table, part = "body", i = condition_index, j = 4, color = "red")
table <- color(table, part = "body", i = condition_index, j = 3, color = "red")


ft <- table %>% 
  width(j = 1:13, width = .5) %>% # Adjust column widths
  merge_v(j = c(1:2)) %>% 
  merge_h(part = "header")

# Create a Word document and add the flextable
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = ft)

# Save the Word document
file_path <- "objects/Clip_evaluation.docx"
print(doc, target = file_path)

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy
#################################################