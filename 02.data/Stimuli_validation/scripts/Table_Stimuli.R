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


table_tot <- summary_tot %>%
  mutate(genuine.acc = ifelse(is.na(genuine.acc.mean) ,"",
                              ifelse(is.na(genuine.acc.sd)|genuine.acc.sd == 0,paste(genuine.acc.mean),
                                     paste(genuine.acc.mean, "±", genuine.acc.sd))),
         genuine.rt = ifelse(is.na(genuine.rt.mean) ,"",
                             ifelse(is.na(genuine.rt.sd)|genuine.rt.sd == 0,paste(genuine.rt.mean),
                                    paste(genuine.rt.mean, "±", genuine.rt.sd))),
         genuine.slider1 = ifelse(is.na(genuine.slider1.mean) ,"",
                                  ifelse(is.na(genuine.slider1.sd)|genuine.slider1.sd == 0,paste(genuine.slider1.mean),
                                         paste(genuine.slider1.mean, "±", genuine.slider1.sd))),
         genuine.slider0 = ifelse(is.na(genuine.slider0.mean) ,"",
                                  ifelse(is.na(genuine.slider0.sd)|genuine.slider0.sd == 0,paste(genuine.slider0.mean),
                                         paste(genuine.slider0.mean, "±", genuine.slider0.sd))),
         emotion.acc = ifelse(is.na(emotion.acc.mean) ,"",
                              ifelse(is.na(emotion.acc.sd)|emotion.acc.sd == 0,paste(emotion.acc.mean),
                                     paste(emotion.acc.mean, "±", emotion.acc.sd))),
         emotion.slider1 = ifelse(is.na(emotion.slider1.mean) ,"",
                                  ifelse(is.na(emotion.slider1.sd)|emotion.slider1.sd == 0,paste(emotion.slider1.mean),
                                         paste(emotion.slider1.mean, "±", emotion.slider1.sd))),
         emotion.slider0 = ifelse(is.na(emotion.slider0.mean) ,"",
                                  ifelse(is.na(emotion.slider0.sd)|emotion.slider0.sd == 0,paste(emotion.slider0.mean),
                                         paste(emotion.slider0.mean, "±", emotion.slider0.sd)))) %>%
  select(emotion,elicitation,file,
         genuine.acc,genuine.rt,genuine.slider1,genuine.slider0,
         emotion.acc,emotion.slider1,emotion.slider0)

table <- table_tot%>% 
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 7)%>% 
  set_header_labels(values = list(
    emotion = "Emotion",
    elicitation = "Elicitation",
    file = "Mp4",
    genuine.acc = "Accuracy",
    genuine.rt = " Rt ",
    emotion.acc = "Accuracy",
    genuine.slider1 = "Slider 1",
    genuine.slider0 = "Slider 0",
    emotion.slider1 = "Slider 1",
    emotion.slider0 = "Slider 0")) %>% 
  add_header_row(values = c(paste("Pts =",n_pts),"",paste( "mp4 = ",n_clips), "Genuine", "Genuine","Genuine", "Genuine",
                            "Emotion","Emotion","Emotion")) %>%  
  merge_v(j = c(1:3)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all")%>% 
  width(j = 1:10, width = .8)

# condition_index <- which(summary_tot[[ncol(summary_tot)]])
# table <- color(table, part = "body", i = condition_index, j = 4, color = "red")
# table <- color(table, part = "body", i = condition_index, j = 3, color = "red")


# Create a Word document and add the flextable
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = table)

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