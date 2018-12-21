library(dplyr)

# Load RTHweb CSV with GBA/BRP survival status (in date format, if NA then patient is alive)
rth <- read.csv("data/regi_via_rthweb_16112018_with_gba_anon.csv", header=TRUE, dec=",", stringsAsFactors = FALSE)
cat(sprintf("\n\nRth: total rows #: %d\n", nrow(rth)))
cat(sprintf("Rth: distinct patients #: %d\n", length(unique(rth$Anoniem.nummer))))


# Remove columns which only has NA as values (some error with dataset contained nameless empty columns)
rth <- rth[,colSums(is.na(rth))<nrow(rth)]

#NOTE THORAXWAND CODE 309, AND MAMMAE NOT USED.
rth <- rth[rth$code_localisatie_doelgebied == 201 | rth$code_localisatie_doelgebied == 303 | rth$code_localisatie_doelgebied == 304  | rth$code_localisatie_doelgebied == 305  | rth$code_localisatie_doelgebied == 306  | rth$code_localisatie_doelgebied == 309 | rth$code_localisatie_doelgebied == 310, ]

# create subset of the 3x18, 5x11, 8x7,5 and 12x5 schedules
rth <- rth[rth$cum_aantal_fracties == 3 | rth$cum_aantal_fracties == 5 | rth$cum_aantal_fracties == 8 | rth$cum_aantal_fracties == 12, ]
cat(sprintf("Rth: distinct patients after subsetting localization area codes [201, 303, 304, 305, 306, 309, 301]  & fractioning schema 3,5,8,12 #: %d\n", length(unique(rth$Anoniem.nummer))))

# Delete all rows where patient number, clin_location, start, and end cumulative dosis are the same (aka almost duplicate rows)
rth <- rth[!duplicated(rth[c("Anoniem.nummer","clin_loc", "startdatum_cum_dosis", "einddatum_cum_dosis", "cum_aantal_fracties", "cum_dosis_specificatie")]),]
cat(sprintf("Rth: removed rows where anoniem.nummer, clin_loc, startdatum_cum_dosis, einddatum_cum_dosis, cum_aantal_fracties, cum_dosis_specificatie are the same \n (For duplicates with single column variation) %d\n", length(unique(rth$Anoniem.nummer))))



# update clin_class to lowercase so that regex extraction will be easier.
rth$clin_class <- tolower(rth$clin_class)

# create new TNM stage columns to extract values from clin_class.
rth["cT_rth"] <- NA
rth["cN_rth"] <- NA
rth["cM_rth"] <- NA

# extract TNM-staging into separate columns by regex
rth <- extract(rth, clin_class, remove = FALSE, into = c('cT_rth'), regex = 't(.*?)n')
rth <- extract(rth, clin_class, remove = FALSE, into = c('cN_rth'), regex = 'n(.*?)m')
rth <- extract(rth, clin_class, remove = FALSE, into = c('cM_rth'), regex = 'm(.*?)$')



# If datum_overlijden_vlg_gba is NA then update value to 10-30-2018 as.date (date of export). these are all the patients still alive.
rth$datum_overlijden_vlg_gba <- ifelse(is.na(rth$datum_overlijden_vlg_gba) | rth$datum_overlijden_vlg_gba == "", "10-30-2018", rth$datum_overlijden_vlg_gba)



# Changing slash in date format to hyphon for easier conversion.
rth$datum_overlijden_vlg_gba <- gsub("/", "-", rth$datum_overlijden_vlg_gba)
rth$einddatum_cum_dosis <- gsub("/", "-", rth$einddatum_cum_dosis)

# All dates are in dd/mm/yyyy format but may be seperated by either slashes or hyphons. Turning all slashes into hyphons.
rth$datum_overlijden_vlg_gba <- as.Date(rth$datum_overlijden_vlg_gba, format = "%m-%d-%Y")
rth$einddatum_cum_dosis <- as.Date(rth$einddatum_cum_dosis, format = "%m-%d-%Y")



# Create unique ID per row. Required later for treatment identifier.
rth["treatmentRow"] <- seq.int(nrow(rth))
rth["diff_days_castor"] <- NA
# Rows with mark value 1 is the treatment selected for that patient (in case of multiple treatments)
rth["mark"] <- NA


# CASTOR DATASET IS REQUIRED FROM HERE ON. (From here we're selecting the most recent/relevant treatment of RTHweb to join/fuse with a Castor record)
# Selecting all patientIds and stopXRT (date treatment stopped/finished)
castor_stopXRT <- castor[,c("Anoniem.nummer", "stopXRT")]
# Add the Castor stopXRT  column to RTHweb records by joining on patientID. (This so we can compare castor stopXRT date with RTH stopXRT date)
rth <- left_join(rth, castor_stopXRT, by = "Anoniem.nummer")
rm(castor_stopXRT) # object no longer required. removing.

# subset all patientID's which occur more than once (duplicates on 1 column). PatientID duplicate equals multiple treatment records of that patient
rthDup <- rth[duplicated(rth$Anoniem.nummer), c("Anoniem.nummer")]

# subset all patients which have Multiple Treatments (MT)
rthMT <- rth[rth$Anoniem.nummer %in% rthDup, ]
# Subset all patients which only have a single treatment (ST)
rthST <- rth[!rth$Anoniem.nummer %in% rthDup, ]
rm(rthDup) # object no longer required. removing

# Subset all treatments from MT object where stopXRT is NOT NA
rthMT_stopXRT <- rthMT[which(!is.na(rthMT$stopXRT)), ]
# Subset all treatments from MT object where stopXRT is NA
rthMT_stopXRT_NA <- rthMT[which(is.na(rthMT$stopXRT)), ]
rm(rthMT) # object no longer required. removing.


# Selecting most recent RTH record compared to Castor record and all other records that are within 14 days of the castor record. AKA selecting all RTHweb treatmen records related to Castor treatment record by days proximity logic.)
# For each patientID in rthMT_stopXRT (where stopXRT is NOT NA)
for(rthId in rthMT_stopXRT$Anoniem.nummer){
  # Subset all RTHweb treatment records of looped patient (which has multiple treatments and stopXRT is NOT NA)
  rthPatient <- rthMT_stopXRT[which(rthMT_stopXRT$Anoniem.nummer == rthId), ]
  # For all RTHweb records. Calculate amount of days between Castor stopXRT and RTHweb einddatum_cum_dosis (which is the same as stopXRT, just named differently)
  rthPatient$diff_days_castor <- difftime(rthPatient$stopXRT, rthPatient$einddatum_cum_dosis, units = c("days"))
  # Subset all records where RTHweb treatment records are within 14 days of Castor treatment record. (AKA the castor and rthweb treatments are either the same or relate to the same treatment by days proximity logic)
  rthPatient <- rthPatient[which(rthPatient$diff_days_castor <= 14 & rthPatient$diff_days_castor >= -14),]
  
  # Select rows by treatmentRow value where diff_days_castor value is closest to 0. If Positive and negative values are both equally close to zero (such as -2 and 2) then negative value is selected (which is future date from castor)
  rthPatientTreatmentRow <- rthPatient[which.min(abs(rthPatient$diff_days_castor - 0)), "treatmentRow"]
  
  # Mark selected treatment records of rth object (marked treatment records are the ones joined with Castor. If its multiple RTHweb records, they will later in this code be fused into one record where the most recent is leading)
  rth$mark[rth$treatmentRow == rthPatientTreatmentRow] <- 1
}


# Selecting most recent RTHweb treatment from patients with multiple treatments where Castor STOPXRT is NA (Patients with multiple treatments but Castor StopXRT NA now have 1 RTHweb treatment selected)
# For each patientID in rthMT_stopXRT (where stopXRT is NA)
for(rthId in rthMT_stopXRT_NA$Anoniem.nummer){
  # Subset all RTHweb treatment records of looped patient (which has multiple treatments and stopXRT is NA)
  rthPatient <- rthMT_stopXRT_NA[which(rthMT_stopXRT_NA$Anoniem.nummer == rthId), ]
  # Order records by most recent RTHweb einddatum_cum_dosis
  rthPatient <- rthPatient[rev(order(rthPatient$einddatum_cum_dosis)),]
  # Store treatmentRow value (This is the auto increment ID artificially created earlier) of the first row of the subset (which is the most recent one due to above ordering)
  rthPatientTreatmentRow <- rthPatient[1:1, "treatmentRow"]
  # Mark RTHweb treatment row in RTH object which was selected in the line above.
  rth$mark[rth$treatmentRow == rthPatientTreatmentRow] <- 1 
}


# For all RTHweb patients with a single treatment. Mark that RTHweb treatment. (This just simplifies code later on. so we can say, only select marked treatments)
for(rthSTTreatmentRow in rthST$treatmentRow){
  rth$mark[rth$treatmentRow == rthSTTreatmentRow] <- 1 
}


rm(rthST, rthMT_stopXRT, rthMT_stopXRT_NA, rthId, rthPatientTreatmentRow, rthSTTreatmentRow, rthPatient) # object no longer required. removing.


# Select all RTHweb treatment records which we marked in the For loops as most recent or relevant to the Castor treatment record.
rth <- rth[which(rth$mark == 1), ]

# ALL LEFTOVER RTHWEB TREATMENT RECORDS ARE NOW RELATED TO THE CASTOR TREATMENT RECORD (multiple RTHweb records for Castor records still exist, theyre just the relevant leftover ones now)


# All RTH rows which were not selected
#rth_mark <- rth[which(is.na(rth$mark)), ]

cat(sprintf("Rth: delete treatment records per patient where RTH treatment is more than 7 days off #: %d\n", length(unique(rth$Anoniem.nummer))))

