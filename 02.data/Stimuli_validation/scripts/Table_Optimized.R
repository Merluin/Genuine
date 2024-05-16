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


# Data cleaning
bad_clips <- c("29_rg_2.mp4","19_rg_5.mp4","16_rg_2.mp4",
               "1_rs_3.mp4", "10_rs_4.mp4","19_rs_3.mp4",
               "29_pg_1.mp4", "3_pg_1.mp4", "15_pg_2.mp4", 
               "55_ps_2.mp4", "20_ps_3.mp4", 
               "9_fg_1.mp4", "40_fg_1.mp4", "45_fg_2.mp4",
               "44_fs_1.mp4","49_fs_2.mp4", "22_fs_1.mp4")
dataset_pulito <- dataset %>%
  filter( !file %in% bad_clips)

# Data summarization for participants
summary_participants <- dataset_pulito %>%
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

# Data summarization for emotions
summary_emotion <- summary_participants %>%
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
summary_tot <- summary_participants %>%
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
                        summary_emotion[3,])

table_tot <- final_dataset %>%
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
  select(emotion,elicitation,
         genuine.acc,genuine.rt,genuine.slider1,genuine.slider0,
         emotion.acc,emotion.slider1,emotion.slider0)

# Count participants and clips
n_pts = length(unique(dataset$subject_id))
n_clips = length(unique(dataset_pulito$file))-1

# Create and format the flextable
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
  add_header_row(values = c(paste("Pts =",n_pts),"", "Genuine", "Genuine","Genuine", "Genuine",
                            "Emotion","Emotion","Emotion")) %>%  
  merge_v(j = c(1:2)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all") %>% 
  width(j = 1:9, width = .8) %>% 
  add_footer_lines(values = paste0("Eliminated clips = ",paste(bad_clips, collapse = ", ")))%>%
  fontsize(part = "footer", size = 7) %>%
  bold( part = "body", i = c(3, 6, 9), bold = TRUE) %>%
  set_caption("Clips Validation")



# Creating a Word document with the table
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = table)
file_path <- "objects/table_optimized.docx"
print(doc, target = file_path)

# Saving on objects in RData format
save(dataset, 
     dataset_pulito,
     summary_participants,
     summary_emotion,
     summary_tot,
     table_tot,
     table,
     file = "data/validation.RData")

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy optimized
#################################################