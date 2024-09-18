###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        03/06/2023
#  Description: General script
#  Experiment   Genuine
#
#  Update:      18/09/2024
###########################################################################

rm(list=ls()) # remove all objects

# Functions ---------------------------------------------------------------
devtools::load_all()

# Pre-processing-----------------------------------------------------------
# replace_csv("education",NA)
run_script("scripts/Dataset.R") # description in /docs/Dataset_description.html
run_script("scripts/Export_xlsx.R") # return data/Genuine_summary_full_pts.xlsx

# statistics
run_script("scripts/EAD_statistics.R") 
run_script("scripts/EIJ_statistics.R") 
run_script("scripts/Demography_statistics.R") 

#################################################
# 
# END
#
#################################### main_script