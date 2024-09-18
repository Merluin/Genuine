questionnaires <- function(filename,subjectorder,sep_)
  
{

# needed packages
library(tidyverse)
library(anytime)
library(readr)
library(car)

#load data

dati <- read.csv(filename, sep= sep_) %>%
  data.frame()

## Preparing dataframe
#### Removing unused variables
dati <- dati[,-1] 
rownames(dati) <- c()

IRI <- paste0("iri_", 1:28)
TAS <- paste0("tas_", 1:20)
colnames(dati)[7:34] <- IRI #rinomino item IRI
colnames(dati)[35:54] <- TAS #rinomino item TAS
colnames(dati)[1] <- "date"
colnames(dati)[2] <- "subject"
colnames(dati)[3] <- "group"
colnames(dati)[4] <- "gender"
colnames(dati)[5] <- "age"
colnames(dati)[6] <- "hand"

ordine.corretto<- dati%>%
  drop_na(subject) %>%
  pull(subject)

dati<-dati[ordine.corretto,]

Pt.id <- 1 : nrow(dati)
dati<-cbind(Pt.id,dati)

# Crearing ID factor for repeated measure analysis

dati$Pt.id <- as.factor(dati$Pt.id)

rownames(dati) <- c()
#dati <- dati[,-c(2,3)]

# Recode the IRI columns and convert them back to numeric
dati$iri_3 <- as.numeric(recode(dati$iri_3, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_4 <- as.numeric(recode(dati$iri_4, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_7 <- as.numeric(recode(dati$iri_7, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_12 <- as.numeric(recode(dati$iri_12, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_13 <- as.numeric(recode(dati$iri_13, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_14 <- as.numeric(recode(dati$iri_14, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_15 <- as.numeric(recode(dati$iri_15, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_18 <- as.numeric(recode(dati$iri_18, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))
dati$iri_19 <- as.numeric(recode(dati$iri_19, "1 = '5'; 2 = '4'; 3 = '3'; 4 = '2'; 5 = '1'"))

# Ensure all IRI columns are numeric
dati[, IRI] <- lapply(dati[, IRI], function(x) as.numeric(as.character(x)))

# Calculate the total sum for IRI
iri_tot <- apply(dati[, IRI], 1, sum)


# For IRI, scoring of subscales is possibile too. Subscales are:  
#   
#   - **Fantasy** Items: 1,5,7,12,16,23,26
# - **Perspective Taking** Items: 3,8,11,15,21,25,28
# - **Empathic Concern** Items: 2,4,9,14,18,20,22
# - **Personal Distress** Items: 10,13,17,19,24,27  

fantasy <- apply(dati[,IRI[c(1,5,7,12,16,23,26)]],1,sum)
perspective_taking <- apply(dati[,IRI[c(3,8,11,15,21,25,28)]],1,sum)
empathic_concern <- apply(dati[,IRI[c(2,4,9,14,18,20,22)]],1,sum)
personal_distress <- apply(dati[,IRI[c(6,10,13,17,19,24,27)]],1,sum)

#TAS
dati$tas_4 <- recode(dati$tas_4, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_5 <- recode(dati$tas_5, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_10 <- recode(dati$tas_10, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_18 <- recode(dati$tas_18, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")
dati$tas_19 <- recode(dati$tas_19, "1 = '5'; 2 = '4'; 3='3'; 4 = '2'; 5='1'")

tas_tot <- apply(dati[,TAS],1,sum)

dati <- cbind(dati,iri_tot, fantasy, perspective_taking, personal_distress, empathic_concern, tas_tot)
attach(dati)

scoring <- data.frame(Pt.id,group,gender,age,hand,fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

detach(dati)


return(dati)
}


# write.csv(dati, "Data/Questionnaires_cleaned.csv")
# # write.csv(scoring, "Scoring/Datasets_Scoring/scoring_questionari.csv")
# save(Questionnaires, file ="DATA/Questionnaires_cleaned.rda")
# # save(scoring, file = "Scoring/Datasets_Scoring/scoring_questionari.rda")
# # save(iri_tot,fantasy,empathic_concern,perspective_taking, personal_distress, file = "Scoring/Datasets_Scoring/iri.rda")
# # save(tas_tot, file = "Scoring/Datasets_Scoring/tas.rda")


###########################################################################
#                                   END                                   #
###########################################################################