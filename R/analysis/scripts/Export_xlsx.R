###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        20/05/2024
#  Description: dataset export to xlsx
#
#  Update:      18/09/2024
###########################################################################

# Clearing workspace
rm(list = ls())  # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all()  # Load necessary functions and packages

# Data loading and eliminate outliers and 5 of the 6 M1 group
# Data loading
load("data/psychopy_dataset.RData") 



wb <- createWorkbook()
# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "d_prime")
# Write data to the corresponding sheet
writeData(wb, sheet = "d_prime", d_wide)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "criterion")
# Write data to the corresponding sheet
writeData(wb, sheet = "criterion", c_wide)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "ΓAED")
# Write data to the corresponding sheet
writeData(wb, sheet = "ΓAED", ΓAED)

# addWorksheet(wb, sheetName = "accuracy.emotion")
# # Write data to the corresponding sheet
# writeData(wb, sheet = "accuracy.emotion", emotion_accuracy)
# 
# addWorksheet(wb, sheetName = "accuracy.genuine")
# # Write data to the corresponding sheet
# writeData(wb, sheet = "accuracy.genuine", genuine_accuracy)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "EA")
# Write data to the corresponding sheet
writeData(wb, sheet = "EA", EA)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "slider.genuine")
# Write data to the corresponding sheet
writeData(wb, sheet = "slider.genuine", genuine_intensity)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "slider.emotion")
# Write data to the corresponding sheet
writeData(wb, sheet = "slider.emotion", emotion_intensity)

# Add a new sheet with the formatted name
addWorksheet(wb, sheetName = "demography")
# Write data to the corresponding sheet
writeData(wb, sheet = "demography", demography)

# Save the workbook
saveWorkbook(wb, "data/Genuine_summary_full_pts.xlsx", overwrite = TRUE)

#################################################
# 
# END
#
#################################################
#  Script for Genuine Study - xlsx 
#################################################