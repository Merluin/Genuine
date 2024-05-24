###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
# Description: Preprocess and filter data, then generate .csv and .xlsx files
#
#  Update:      16/05/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages
#replace_csv("session",002) 

# Data loading
load("data/dataset.RData") 

# Count participants
n <- length(unique(dataset$participant)) # Calculate the number of unique participants

data <- dataset # Avoid overwriting existing object

for(i in 1:n){
pts <- sprintf("%02d", i)
n_clips <- length(unique(dataset$file))-1

dataset <- data %>%
  filter(participant == i)

group <- unique(dataset$group)

# Data summarization for participants
summary_participants <- dataset %>%
  filter(Trials_loop.thisRepN == 0) %>%
  mutate(genuine_slider.response = abs(genuine_slider.response)) %>%
  group_by( participant, emotion, elicitation, session) %>%
  summarize_data()

# Data summarization for emotions
summary_emotion <- summary_participants %>%
  mutate(elicitation = "Total") %>%
  group_by( emotion,elicitation, session) %>%
  summarize_summary()

# Data summarization for emotion and elicitation
summary_tot <- summary_participants %>%
  group_by( emotion, session, elicitation) %>%
  summarize_summary()

# Summarize data by emotion, session, and elicitation
summary_tot <- summary_participants %>%
  select( -participant)

# Combine emotion and total summaries and arrange them
final_dataset <- bind_rows(summary_emotion, summary_tot) %>%
  mutate(elicitation = factor(elicitation, levels = c("genuine", "paused", "Total"))) %>%
  arrange(emotion, session, elicitation)

# prepare data for long format file
long_format <-summary_emotion %>% 
  mutate(genuine.accuracy = genuine.acc.mean,
          genuine.rt = genuine.rt1.mean,
          genuine.slider.correct = genuine.slider1.mean,
          genuine.slider.incorrect = genuine.slider0.mean,
          emotion.accuracy = emotion.acc.mean,
          emotion.slider.correct = emotion.slider1.mean,
          emotion.slider.incorrect = emotion.slider0.mean,
          subject = i) %>%
  select(subject,session, emotion,
         genuine.accuracy,genuine.rt,
         genuine.slider.correct,genuine.slider.incorrect,
         emotion.accuracy,
         emotion.slider.correct,emotion.slider.incorrect)

  long_format$group <- group
 # pts <- pts + 100  # Ensure the order is sequential from 1, 2, 3,..., 37 instead of 1, 10, 11,..., 37

  # Save data frames
  write.xlsx(final_dataset, paste0("data/Pt_",pts,"_",group,".xlsx"))
  write.csv(long_format, paste0("data/Pt_", pts, "_", group, ".csv"))

}

# Concatenate new lf.csv files
dataset_long <- list.files(path="data",pattern="\\.csv$", full.names = TRUE) %>%
  lapply(.,function(x) read.csv(x, sep=",", header=TRUE,stringsAsFactors = FALSE ))%>%
  bind_rows(.id = "subject") %>%
  select(-X)

# Save the concatenated data frame
write.xlsx(dataset_long, paste0("data/dati_summary_lf.xlsx"))

#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Preprocessing
#################################################