#library(tidyr) #this is used somewhere. cant remember which prepare file.
# Combining all datasets into one dataframe

source("prepare/prepare_anonkey") #Must load castor anon_key first as object is used in prepare_castor.r process
source("prepare/prepare_castor.r")
source("prepare/prepare_rth.r")
source("prepare/prepare_dicom.r")

# Inner join castor and RTH by patientID (anoniem.nummer)
castor_rth <- inner_join(castor, rth, by = "Anoniem.nummer")
# Inner join combined Castor & RTH with dicom data by patientID (anoniem.nummer)
data <- inner_join(castor_rth, dicom, by = "Anoniem.nummer")
rm(castor_rth, dicom) # Ojbects no longer required. Removing.
