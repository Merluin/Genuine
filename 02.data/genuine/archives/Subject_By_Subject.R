###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        23/01/2024
#  Description: Pilot accuracy subject table
#
#  Update:      23/01/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts


# Load dependencies
devtools::load_all() # Load necessary functions and packages
# replace_csv("session",003) pt 5 session 3 was set to 1

# Data loading
load("data/dataset.RData") 

# Count participants and clips
n <- length(unique(dataset$participant)) # Calculate the number of unique participants



# Template data frame creation
# Create data frames for both groups with base structure
GAcc <- GAcc.slider <- GRt <- Acc.slider <- Acc <- create_emotion_df("Experimental")
GAcc_control <- GAcc.slider_control <- GRt_control <- Acc.slider_control <- Acc_control <- create_emotion_df("Control")
GAcc_m1 <- GAcc.slider_m1 <- GRt_m1 <- Acc.slider_m1 <- Acc_m1 <- create_emotion_df("M1")

# Initialize participant counters for each group
pts <- 0
pts_ctl <- 0
pts_m1 <- 0

# Loop over each participant for data summarization
for(i in 1:n){
  # Filter dataset for the current participant
  dataset2 <- dataset %>%
  filter(participant == i)

  # Perform data summarization for this participant
  summary_participants <- dataset2 %>%
    group_by( participant, emotion, elicitation, session, group) %>%
    summarize_data()

  # Summarize data by emotion, elicitation, and session with a 'Total' elicitation
  summary_emotion <- summary_participants %>%
    mutate(elicitation = "Total") %>%
    group_by( emotion,elicitation, session, group) %>%
    summarize_summary()

  # Summarize data by emotion, session, and elicitation
  summary_tot <- summary_participants %>%
    select( -participant)

  # Combine emotion and total summaries and arrange them
  final_dataset <- bind_rows(summary_emotion, summary_tot) %>%
    mutate(elicitation = factor(elicitation, levels = c("genuine", "paused", "Total"))) %>%
    arrange(emotion, session, elicitation, group)


  # Check group and bind data to respective group data frame
  if(any(final_dataset$group == "Experimental")){
    pts <-  pts + 1
    GAcc <- cbind(GAcc,final_dataset$genuine.acc.mean)
    names(GAcc)[ncol(GAcc)] <- paste0("pt", pts)
    
    GAcc.slider<- cbind(GAcc.slider,abs(final_dataset$genuine.slider1.mean))
    names(GAcc.slider)[ncol(GAcc.slider)] <- paste0("pt", pts)
    
    GRt <- cbind(GRt,final_dataset$genuine.rt1.mean)
    names(GRt)[ncol(GRt)] <- paste0("pt", pts)
    
    Acc.slider <- cbind(Acc.slider,final_dataset$emotion.slider1.mean)
    names(Acc.slider)[ncol(Acc.slider)] <- paste0("pt", pts)
    
    Acc <- cbind(Acc,final_dataset$emotion.acc.mean)
    names(Acc)[ncol(Acc)] <- paste0("pt", pts)

  } else if(any(final_dataset$group == "Control")) {
    pts_ctl <- pts_ctl + 1
    GAcc_control <- cbind(GAcc_control,final_dataset$genuine.acc.mean)
    names(GAcc_control)[ncol(GAcc_control)] <- paste0("pt", pts_ctl)
    
    GAcc.slider_control <- cbind(GAcc.slider_control,abs(final_dataset$genuine.slider1.mean))
    names(GAcc.slider_control)[ncol(GAcc.slider_control)] <- paste0("pt", pts_ctl)
    
    GRt_control <- cbind(GRt_control,final_dataset$genuine.rt1.mean)
    names(GRt_control)[ncol(GRt_control)] <- paste0("pt", pts_ctl)
    
    Acc.slider_control <- cbind(Acc.slider_control,final_dataset$emotion.slider1.mean)
    names(Acc.slider_control)[ncol(Acc.slider_control)] <- paste0("pt", pts_ctl)
    
    Acc_control <- cbind(Acc_control,final_dataset$emotion.acc.mean)
    names(Acc_control)[ncol(Acc_control)] <- paste0("pt", pts_ctl) 
    
  } else if(any(final_dataset$group == "M1")) {
    pts_m1 <- pts_m1 + 1
    GAcc_m1 <- cbind(GAcc_m1,final_dataset$genuine.acc.mean)
    names(GAcc_m1)[ncol(GAcc_m1)] <- paste0("pt", pts_m1)
    
    GAcc.slider_m1 <- cbind(GAcc.slider_m1,abs(final_dataset$genuine.slider1.mean))
    names(GAcc.slider_m1)[ncol(GAcc.slider_m1)] <- paste0("pt", pts_m1)
    
    GRt_m1 <- cbind(GRt_m1,final_dataset$genuine.rt1.mean)
    names(GRt_m1)[ncol(GRt_m1)] <- paste0("pt", pts_m1)
    
    Acc.slider_m1 <- cbind(Acc.slider_m1,final_dataset$emotion.slider1.mean)
    names(Acc.slider_m1)[ncol(Acc.slider_m1)] <- paste0("pt", pts_m1)
    
    Acc_m1 <- cbind(Acc_m1,final_dataset$emotion.acc.mean)
    names(Acc_m1)[ncol(Acc_m1)] <- paste0("pt", pts_m1) 
  }
}


# Perform row means
GAcc <- add_pt_row_means(GAcc)
GAcc.slider <- add_pt_row_means(GAcc.slider)
GRt <- add_pt_row_means(GRt)
Acc.slider <- add_pt_row_means(Acc.slider)
Acc <- add_pt_row_means(Acc)
GAcc_control <- add_pt_row_means(GAcc_control)
GAcc.slider_control <- add_pt_row_means(GAcc.slider_control)
GRt_control <- add_pt_row_means(GRt_control)
Acc.slider_control <- add_pt_row_means(Acc.slider_control)
Acc_control <- add_pt_row_means(Acc_control)
GAcc_m1 <- add_pt_row_means(GAcc_m1)
GAcc.slider_m1 <- add_pt_row_means(GAcc.slider_m1)
GRt_m1 <- add_pt_row_means(GRt_m1)
Acc.slider_m1 <- add_pt_row_means(Acc.slider_m1)
Acc_m1 <- add_pt_row_means(Acc_m1)

# Combine control and test group data frames for each metric
GAcc <- plyr::rbind.fill(GAcc, GAcc_control, GAcc_m1)
GAcc.slider <- plyr::rbind.fill(GAcc.slider,GAcc.slider_control, GAcc.slider_m1)
GRt <- plyr::rbind.fill(GRt,GRt_control, GRt_m1)
Acc.slider <- plyr::rbind.fill(Acc.slider,Acc.slider_control, Acc.slider_m1)
Acc <- plyr::rbind.fill(Acc,Acc_control, Acc_m1)


#save data.frame
file_path <- "objects/Subjects_Accuracy.xlsx" # Example with dynamic date

# Create a new workbook
wb <- createWorkbook()

# Add sheets and write data frames to each sheet
addWorksheet(wb, "Genuine.Accuracy")
writeData(wb, "Genuine.Accuracy", GAcc)
addWorksheet(wb, "Genuine.slider")
writeData(wb, "Genuine.slider", GAcc.slider)
addWorksheet(wb, "Genuine.Rt")
writeData(wb, "Genuine.Rt", GRt)
addWorksheet(wb, "Emotion.slider")
writeData(wb, "Emotion.slider", Acc.slider)
addWorksheet(wb, "Emotion.Accuracy")
writeData(wb, "Emotion.Accuracy", Acc)

# Save the workbook
saveWorkbook(wb, file_path, overwrite = TRUE)

# plots
genuine_pts_plot <- GAcc %>%
  gather("Participant","Accuracy",contains("pt")) %>%
  mutate(Session = as.factor(Session),
         Emotion = as.factor(Emotion),
         Elicitation = as.factor(Elicitation),
         Accuracy = Accuracy*100) %>%
  ggplot(aes(x = Session, y = Accuracy, group = Participant, color = Participant)) +
  geom_point() +
  geom_line() +
  stat_summary() +
  facet_grid(Emotion*Group ~ Elicitation) +
  theme_minimal() + theme(legend.position="none")
  
# plots
genuine_plot <- GAcc %>%
  gather("Participant","Accuracy",contains("pt")) %>%
  mutate(Session = as.factor(Session),
         Emotion = as.factor(Emotion),
         Elicitation = as.factor(Elicitation),
         Accuracy = Accuracy*100) %>%
  group_by(Emotion, Session, Elicitation, Group) %>%
  summarise(Accuracy = mean(Accuracy, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = Session, y = Accuracy,group= Group, color = Group)) +
  geom_point() +
  geom_line() +
  stat_summary() +
  facet_grid(Emotion ~ Elicitation) +
  theme_minimal()


#################################################
# 
# END
#
#################################################
#  Script for GENUINE Study - Pilot accuracy subject table
#################################################