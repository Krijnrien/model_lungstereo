library(dplyr)

# Load datasets into objects, dataset columns are labeled (headers)
dicom <- read.csv("data/updated_dicom_combined_3.csv", header=TRUE, dec=",", stringsAsFactors=FALSE)
dicom[dicom == ""] <- NA

colnames(dicom)[colnames(dicom) == 'MRN'] <- 'Anoniem.nummer'


# Remove all rows there the name_updated column is empty or NA
dicom <- dicom[!(is.na(dicom$NAME_UPDATED) | dicom$NAME_UPDATED==""), ]

# Add new column with the number of empty columns for each row
dicom <- mutate(dicom, missing_data_points = rowSums(is.na(dicom)))
dicom <- dicom[order(dicom$Anoniem.nummer, -abs(dicom$missing_data_points) ), ] 
dicom <- filter(dicom, missing_data_points < 5)

# Only keep rows with the following labeled names

# Turn every name_updated row per MRN into a new column.
dicom <- reshape(dicom, idvar = "Anoniem.nummer", timevar = "NAME_UPDATED", direction = "wide")


