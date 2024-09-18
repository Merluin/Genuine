#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################
dataset_concatenation <- function(dir,dataset_name) {
  
  # packages
  library(tidyverse)
  
  # find nb of file
  folder_dir <- dir
  
  # concatenate all file
  dataset <- list.files(path=folder_dir, full.names = TRUE) %>%
    lapply(., function(x) {
      df <- read.csv(x, sep=",", header=TRUE, stringsAsFactors = FALSE)
      
      # Convert 'gender' column to character if it exists
      if("gender" %in% names(df)) {
        df$gender <- as.character(df$gender)
      }
      
      return(df)
    }) %>%
    bind_rows(.id = "participant") %>%
    mutate(across(where(is.character), str_remove_all, pattern = "[\\[|\\] ']"))
  
  # Creating a unique subject identifier
  dataset <- dataset %>%
    mutate(subject_id = as.numeric(as.factor(paste(participant, date, sep = "_"))))
  
  
  # save data
  save(dataset, file= paste0("data/", dataset_name, ".RData"))
}
#end function  

#################################################
# 
# END
#
#################################################