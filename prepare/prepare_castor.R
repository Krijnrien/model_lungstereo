library(dplyr)
library(tidyr)

# Load castor CSV
castor <- read.csv("data/castor.csv", header=TRUE, dec=",")
cat(sprintf("Castor: Opened csv. Total rows #: %d\n", nrow(castor)))


# All -95, -96, -97, -98, -99 values in every Castor column are Castor codes meaning missing value. Updating them to NA
castor[castor == "-95"] <- NA # -95 "is measurement failed"
castor[castor == "-96"] <- NA # -96 "not applicabe"
castor[castor == "-97"] <- NA # -97 "not asked"
castor[castor == "-98"] <- NA # -98 "asked but unknown"
castor[castor == "-99"] <- NA # -99 "not done"


# RecordId 1 and 236 are completely empty rows
castor <- castor[!castor$RecordId %in% c(1, 236), ]


# Join castor and link_key by RecordID. NOTE: columns might be named slightly different. this method requires them to be exactly the same. (NOTE: Other method I used before called Merge incorrectly joined them)
castor <- inner_join(castor, link_key, by = "RecordId")
cat(sprintf("Castor: distinct patients #: %d\n", length(unique(castor$Anoniem.nummer))))
rm(link_key) # Object no longer required. Removing.

# Patient ID 1708520 and 6770828 refer to each other incorrectly. By updating IDfirstTreatment record they wont be merged as one record.
castor[castor$Anoniem.nummer == 1708520, "IDfirstTreatment"] <- NA
castor[castor$Anoniem.nummer == 6770828, "IDfirstTreatment"] <- NA
#castor <- castor[!castor$Anoniem.nummer %in% c(1708520, 6770828), ]

# TODO
# still has duplicates in final dataset. Probably because these records do not refer ot each other which is how theyre combined, by refer id.
castor <- castor[!castor$Anoniem.nummer %in% c(3609371, 5214409), ]





# Get recordID of row where FirstLStereo is not empty
# Get recordID that column IDfirstTreatment is refering to
## NOTE: Some patients have 2 records in Castor but do not have FirstLStereo or IDfirstTreatment column(s) filled in.
castorRefer <- castor[which(castor$IDfirstTreatment != ""  | !is.na(castor$IDfirstTreatment)), ]
castorRefer <-c(castorRefer$RecordId, castorRefer$IDfirstTreatment)

# Get RecordID which occures more than once in Castor.
castorDup <- castor[duplicated(castor$Anoniem.nummer), c("Anoniem.nummer")]

castorMT_list <-c(castorDup, castorRefer) # Select Castor records of patients with multiple records (aka multiple treatments) and all records which have a IDfirstTreatment value (which refers to other treatment row as either follow-up or different treatment, AKA same patient)
rm(castorDup) # Object no longer required. Removing.

#castorMT <- filter(castor, Anoniem.nummer %in% castorDup)
castorMT <- castor[castor$Anoniem.nummer %in% castorMT_list, ]
cat(sprintf("CastorMT: total rows of patients with multiple treatments #: %d\n", nrow(castorMT)))
cat(sprintf("CastorMT: distinct multiple treatments #: %d\n", length(castorMT[!duplicated(castorMT$Anoniem.nummer), c("Anoniem.nummer")])))

castorST <- castor[!castor$Anoniem.nummer %in% castorMT_list, ]
cat(sprintf("CastorST: total rows of patients with single treatments #: %d\n", nrow(castorST)))
rm(castorRefer, castorMT_list)

castorDupMrn <- castorMT[!duplicated(castorMT$Anoniem.nummer),]
castorDupMrn <- castorDupMrn$Anoniem.nummer

cat(sprintf("CastorMT: deleting rows where stopXRT is empty or NA %d\n", nrow(castorMT)))
castorMT <- castorMT[which(castorMT$stopXRT != ""), ]
castorMT <- castorMT[which(!is.na(castorMT$stopXRT)), ]
cat(sprintf("CastorMT: distinct multiple treatments #: %d\n", length(castorMT[!duplicated(castorMT$Anoniem.nummer), c("Anoniem.nummer")])))


# Formatting StopXRT date. Script position is crucial.
# - Above we delete castor Records from castorMT where stopXRT is empty (can only be done on character string, not date types)
# - Below we require it to be date format to calculate difference in days.


# Changing slash date format to hyphon for easier conversion.
castorMT$stopXRT <- gsub("/", "-", castorMT$stopXRT)
# All dates are in dd/mm/yyyy format but may be seperated by either slashes or hyphons. Turning all slashes into hyphons.
castorMT$stopXRT <- as.Date(castorMT$stopXRT, format = "%d-%m-%Y")


# for each Castor record get the most recent and then only keep other treatment records of the same patient within 7 days of the most recent record.
for (mrn in castorDupMrn) {
  # get all treatment records per patient
  castorPatient <- castorMT[which(castorMT$Anoniem.nummer == mrn ), ]
  # order by most recent
  castorPatient <- castorPatient[rev(order(castorPatient$stopXRT)),]
  
  # set most recent castor treatment record in temp1
  temp1 <- castorPatient[1:1,]
  
  # Remove the treatment record set in temp1 from the castorPatient dataframe. Otherwise we would select it twice.
  castorPatient <- castorPatient[!castorPatient$RecordId %in% temp1$RecordId, ]
  # Calculate differenc in days between most recent treatment record (temp1) and all other treatment records.
  castorPatient$diff_in_days_maintreatment <- difftime(temp1$stopXRT, castorPatient$stopXRT, units = c("days"))
  
  # Remove all treatment records which are not within 7 days of most recent treatment record (temp1)
  castorPatient_treatment_too_old <- castorPatient[which(castorPatient$diff_in_days_maintreatment > 14), c("RecordId")]
  castorMT <- castorMT[!castorMT$RecordId %in% castorPatient_treatment_too_old, ]
}



rm(temp1, castorPatient, castorDupMrn, castorPatient_treatment_too_old, mrn)


castorMT$stopXRT <- as.character(castorMT$stopXRT)

# Combine castor record and the castor record it refers to with column IDfirstTreatment into one record.
# If column IDfirstTreatment is empty it's skipped and stays as it is. 
castorMT <- castorMT %>% 
  mutate_all(function(i) replace(i, i == '', NA)) %>% 
  group_by(RecordId_new = replace(RecordId, !is.na(IDfirstTreatment), IDfirstTreatment[!is.na(IDfirstTreatment)])) %>% 
  fill(-RecordId, .direction = 'up') %>% 
  fill(-RecordId) %>% 
  ungroup() %>% 
  filter(!RecordId %in% IDfirstTreatment)

cat(sprintf("CastorMT: Selected most recent treatment record per patient.\n Deleting all other patient treatment records older than 7 days of the selected most recent treatment.\n Combined leftover treatment records as one record. #: %d\n", nrow(castorMT[!duplicated(castorMT$Anoniem.nummer),])))

# Combine single treatment and multiple (which is now also single treatment) back into one dataframe.
castorST = dplyr::rename(castorST, RecordId_new = RecordId)
castorMT[,"RecordId"]=NULL
rm(castor)
cat("\nCombine CastorST and CastorMT\n")
castor <- rbind(castorST, castorMT)
rm(castorMT, castorST)
cat(sprintf("Castor: New cleaned / restructured distinct castor patients #: %d\n", length(unique(castor$Anoniem.nummer))))





# Changing slash date format to hyphon for easier conversion.
castor$startXRT <- gsub("/", "-", castor$startXRT)
castor$stopXRT <- gsub("/", "-", castor$stopXRT)
# All dates are in dd/mm/yyyy format but may be seperated by either slashes or hyphons. Turning all slashes into hyphons.
castor$startXRT <- as.Date(castor$startXRT, format = "%d-%m-%Y")
castor$stopXRT <- as.Date(castor$stopXRT, format = "%d-%m-%Y")


# Uncomment to Check Castor for duplicates
#castorDup <- castor[duplicated(castor$Anoniem.nummer), "Anoniem.Nummer"]
#castor2 <- castor[castor$Anoniem.nummer %in% castorDup, ]


