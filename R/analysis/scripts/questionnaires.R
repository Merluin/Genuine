#################################################
# 
# Experiment:     QualiaHappy
# Programmer:     Thomas Quettier
# Date:           18/06/2021
# Description:    Questionaires Correlations analysis
#
#################################################

# Clear the Workspace
rm(list=ls()) # Remove all objects from the workspace for a fresh start

# Load Required Functions and Libraries
devtools::load_all() # Load all packages and functions from the local directory

# Questionnaire ----
# Read the CSV file
file <- read.csv("original_data/questionnaires.csv", sep = ";", header = TRUE)

# Ensure the Codice.ID column is read correctly and is numeric
file$Codice.ID <- as.numeric(as.character(file$Codice.ID))

# Arrange the data by Codice.ID
subjectorder <- file %>%
  arrange(Codice.ID) %>%
  drop_na(Codice.ID) %>%
  pull(Codice.ID)

Questionnaires<-questionnaires("original_data/questionnaires.csv",subjectorder,";")


# IRI TAS summary
IRI<-Questionnaires%>%
  select(id, iri_tot)%>%
  drop_na()%>%
  summarise_at(vars(iri_tot), list(mean,sd,max,min))%>%
  'colnames<-'(c( "mean", "Sd","max","min"))
TAS<-Questionnaires%>%
  select(id, tas_tot)%>%
  drop_na()%>%
  summarise_at(vars( tas_tot), list(mean,sd,max,min))%>%
  'colnames<-'(c( "mean", "Sd","max","min"))


Questionnaire<-Questionnaires%>%
  select(id,fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

write.xlsx(Questionnaire, "data/Questionnaire.xlsx", rowNames = FALSE) # Save the table as an Excel file


############### Plot cor ----
jpeg("07.figures/cor_val_exp2.jpg", units="in", width=10, height=8, res=200)
val<-val%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"AR.BLO.HPY", "AR.BLO.NEU", "AR.FRE.HPY", "AR.FRE.NEU","VAL.BLO.HPY", "VAL.BLO.NEU", "VAL.FRE.HPY" ,"VAL.FRE.NEU"))
chart.Correlation(val, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(val[,2],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6)
p<-cor.test(val[,3],val[,11])
p.adjust(p$p.value, method = "fdr", n = 6)
p<-cor.test(val[,4],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI pd cor arousal congruent neutral r .52 p = 0.03
p<-cor.test(val[,5],val[,8])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor arousal congruent neutral r .54 p = 0.023
p<-cor.test(val[,5],val[,10])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor arousal free neutral r .5 p = 0.047
p<-cor.test(val[,5],val[,11])
p.adjust(p$p.value, method = "fdr", n = 6) # IRI TOT cor valence congruent happy r -.5 p = 0.045

jpeg("07.figures/cor_ort_exp2.jpg", units="in", width=10, height=8, res=200)
OR<-OR%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"ORT.BLO.HPY", "ORT.BLO.NEU", "ORT.FRE.HPY", "ORT.FRE.NEU"))
chart.Correlation(OR, histogram=FALSE, pch=19,method ="pearson")
dev.off()


jpeg("07.figures/cor_ip_exp2.jpg", units="in", width=10, height=8, res=200)
IP<-IP%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"IP.BLO.HPY", "IP.BLO.NEU", "IP.FRE.HPY", "IP.FRE.NEU"))
chart.Correlation(IP, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(IP[,2],IP[,9])
p.adjust(p$p.value, method = "fdr", n = 3)
p<-cor.test(IP[,2],IP[,10])
p.adjust(p$p.value, method = "fdr", n = 3) # IRI.PT cor IP.free.neutral r = 0.5 p=0.029
p<-cor.test(IP[,5],IP[,10])  
p.adjust(p$p.value, method = "fdr", n = 3)


jpeg("07.figures/cor_ct_exp2.jpg", units="in", width=10, height=8, res=200)
CT<-CT%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"CT.BLO.HPY", "CT.BLO.MIX", "CT.BLO.NEU", "CT.FRE.HPY","CT.FRE.MIX", "CT.FRE.NEU"))
chart.Correlation(CT, histogram=FALSE, pch=19,method ="pearson")
dev.off()
p<-cor.test(CT[,2],CT[,7])
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,2],CT[,12]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,3],CT[,11]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)
p<-cor.test(CT[,4],CT[,8]) 
p.adjust(p$p.value, method = "bonferroni", n = 4)




#################################################
# 
# END
#
#################################################
