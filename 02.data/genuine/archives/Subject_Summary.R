###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy optimized
#
#  Update:      05/03/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages

# Data loading
load("data/dataset.RData") 

# Count participants and clips
n_pts = length(unique(dataset$participant))
n_clips = length(unique(dataset$file))-1

# Perform data summarization for this participant
summary_participants <- dataset %>%
  group_by( participant, emotion, elicitation, session, group) %>%
  summarize_data()

# Summarize data by emotion, elicitation, and session with a 'Total' elicitation
summary_emotion <- summary_participants %>%
  mutate(elicitation = "Total") %>%
  group_by( emotion,elicitation, session, group) %>%
  summarize_summary()

summary_tot <- summary_participants %>%
  group_by( emotion,elicitation, session, group) %>%
  summarize_summary()

# Combine emotion and total summaries and arrange them
final_dataset <- bind_rows(summary_emotion, summary_tot) %>%
  arrange(desc(group),emotion, session, desc(elicitation) )

#save data.frame
write.xlsx(final_dataset, "objects/summary_subjects.xlsx")

table_tot <- final_dataset %>%
  mutate(genuine.acc = ifelse(is.na(genuine.acc.mean) ,"",
                              ifelse(is.na(genuine.acc.sd)|genuine.acc.sd == 0,paste(genuine.acc.mean),
                                     paste(genuine.acc.mean, "±", genuine.acc.sd))),
         genuine.rt1 = ifelse(is.na(genuine.rt1.mean) ,"",
                             ifelse(is.na(genuine.rt1.sd)|genuine.rt1.sd == 0,paste(genuine.rt1.mean),
                                    paste(genuine.rt1.mean, "±", genuine.rt1.sd))),
         genuine.rt0 = ifelse(is.na(genuine.rt0.mean) ,"",
                              ifelse(is.na(genuine.rt0.sd)|genuine.rt0.sd == 0,paste(genuine.rt0.mean),
                                     paste(genuine.rt0.mean, "±", genuine.rt0.sd))),
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
  select(group, emotion,session,elicitation,
         genuine.acc,genuine.rt1,genuine.rt0, genuine.slider1,genuine.slider0,
         emotion.acc,emotion.slider1,emotion.slider0)


# Create and format the flextable
table <- table_tot%>% 
  flextable() %>% 
  autofit() %>% 
  theme_vanilla() %>% 
  fontsize(part = "all", size = 7)%>% 
  set_header_labels(values = list(
    group = "Group",
    emotion = "Emotion",
    session = "Session",
    elicitation = "Elicitation",
    file = "Mp4",
    genuine.acc = "Accuracy",
    genuine.rt1 = " Rt correct",
    genuine.rt0 = " Rt incorrect",
    emotion.acc = "Accuracy",
    genuine.slider1 = "Slider correct",
    genuine.slider0 = "Slider incorrect",
    emotion.slider1 = "Slider correct",
    emotion.slider0 = "Slider incorrect")) %>% 
  add_header_row(values = c(paste("Pts =",n_pts),"","","", "Genuine","Genuine", "Genuine","Genuine", "Genuine",
                            "Emotion","Emotion","Emotion")) %>%  
  merge_v(j = c(1:4)) %>% 
  merge_h(part = "header") %>% 
  align(align = "center", part = "all") %>% 
  width(j = 1:12, width = .8) %>% 
  bold( part = "body", i = c(3, 6, 9,12,15,18,21,24,27), bold = TRUE) %>%
  set_caption("Summary Participant")


# Printing the table for preview
table


# Creating a Word document with the table
doc <- officer::read_docx()
doc <- body_add_flextable(doc, value = table)
#doc <- doc %>% officer::body_end_section_landscape()
# doc <- body_add_par(doc, value = "", style = "Normal") %>% 
#   slip_in_section(landscape = TRUE, page_size = officer::page_size(orient = "landscape"))

file_path <- "objects/summary_subjects.docx"
print(doc, target = file_path)


#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy optimized
#################################################