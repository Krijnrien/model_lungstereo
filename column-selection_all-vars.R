library(dplyr)
library(plyr)

source("factor_data.R")


# Unusable features such as unique identifiers, dates and zero variance columns
drop = c('', 'X','RecordId_new','Institute.Abbreviation','Record.Creation.Date','BirthYear', 'diagdate', 'History', 'firstLStereo',
         'IDfirstTreatment', 'TreatmentNumber', 'LesionNumber', 'location', 'cT', 'cN', 'cM', 'startXRT', 'stopXRT.x', 'stopXRT.y', 'Week1comment', 'Week2comment',
         'Week3comment', 'Datum1mnd', 'Mnd1comment', 'Datum6mnd', 'Mnd6comment', 'Datum12mnd', 'Mnd12comment',
         'Datum24mnd', 'Mnd24comment', 'LastFUDate', 'survival', 'datumDood', 'remarks', 'CastorCode', 'Anoniem.nummer', 
         
         'geboortedatum', 'datum_overleden_epic', 'datum_overlijden_vlg_gba', 'datum_overlijden_vlg_regi', 'clin_class',
         'cM_rth', 'cN_rth', 'cT_rth', 'clin_loc', 'clin_loc_oms',	'clin_morfologie_oms',	'clin_differentiatie_oms',	'loc_doelgeb',
         'startdatum_cum_dosis', 'einddatum_cum_dosis', 'cum_aantal_fracties', 'last_followup',
         
         'treatmentRow', 'diff_days_castor', 'mark', 
         
         'X.Hart_en_AortaAsc',	'ID.Hart_en_AortaAsc', 'RTSTRUCT_UID.Hart_en_AortaAsc','RTDOSE_UID.Hart_en_AortaAsc', 'NAME.Hart_en_AortaAsc', 'DOSETYPE.Hart_en_AortaAsc', 'SERIESDESCRIPTION.Hart_en_AortaAsc', 'WARNINGS.Hart_en_AortaAsc', 'AANGEMAAKT.Hart_en_AortaAsc', 'CHECKSUM_RTDOSE.Hart_en_AortaAsc', 'missing_data_points.Hart_en_AortaAsc',
         'X.Longen',	'ID.Longen', 'RTSTRUCT_UID.Longen','RTDOSE_UID.Longen', 'NAME.Longen', 'DOSETYPE.Longen', 'SERIESDESCRIPTION.Longen', 'WARNINGS.Longen', 'AANGEMAAKT.Longen', 'CHECKSUM_RTDOSE.Longen', 'missing_data_points.Longen',
         'X.PTV',	'ID.PTV', 'RTSTRUCT_UID.PTV','RTDOSE_UID.PTV', 'NAME.PTV', 'DOSETYPE.PTV', 'SERIESDESCRIPTION.PTV', 'WARNINGS.PTV', 'AANGEMAAKT.PTV', 'CHECKSUM_RTDOSE.PTV', 'missing_data_points.PTV',
         'X.Oes',	'ID.Oes', 'RTSTRUCT_UID.Oes','RTDOSE_UID.Oes', 'NAME.Oes', 'DOSETYPE.Oes', 'SERIESDESCRIPTION.Oes', 'WARNINGS.Oes', 'AANGEMAAKT.Oes', 'CHECKSUM_RTDOSE.Oes', 'missing_data_points.Oes'
)
data = data[ , !(names(data) %in% drop)]


#keep = c()
#data = data[, keep]



data_class = data$survivalstat # place outcome in separate variable
data$survivalstat = NULL #remove outcome from data
data_class = revalue(data_class, c('0' = 'nonEvent','1' = 'event')) # relabel outcome as event and nonEvent: 0:dead before 2yr --> nonEvent, 1:alive after 2yr --> event
data <- list(data,data_class) 