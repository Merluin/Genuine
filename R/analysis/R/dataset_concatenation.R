dataset_concatenation <- function(path,dataset_name) {
  
  # packages
  library(tidyverse)
  
  # find nb of file
  folder_dir <- path
  
  # concatenate all file
  dataset <- list.files(path=folder_dir, full.names = TRUE) %>%
    lapply(., function(x) {
      # Extracting participant ID from file name
      participant_id <- str_extract(basename(x), "\\d+")
      
      # Read the file
      df <- read.csv(x, sep=",", header=TRUE, stringsAsFactors = FALSE)
      
      # Add participant ID to the dataframe
      df$participant <- as.numeric(participant_id)
      
      # Convert 'gender' column to character if it exists
      if("gender" %in% names(df)) {
        df$gender <- as.character(df$gender)
      }
      
      return(df)
    }) %>%
    bind_rows() %>%
    mutate(across(where(is.character), str_remove_all, pattern = "[\\[|\\] ']"))
  
  
  # save data
  save(dataset, file= paste0("data/", dataset_name, ".RData"))
}