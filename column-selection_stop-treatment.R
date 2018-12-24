source("clean_data.r")

# columns to drop
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

# Columns to keep
keep = c('sex', 'WHO', 'PA', 'origin', 'Schedule', 'FEV1', 'size', 'agediag', 'diff_in_days', #Castor (without toxicity)
       'aantal_tumoren',	'aantal_doelgebieden', 'volgnummer_tumor',	'volgnummer_doelgebied',	'volgnummer_radiotherapie', 'code_localisatie_doelgebied', 'opzet_radiotherapie',	'indicatie_bestraling', 'location_stat', 'cT_stat', 'cN_stat', 'cM_stat', 'survivalstat','cum_dosis_specificatie', #RTHWEB
        'VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc',  'V65.Hart_en_AortaAsc',  'V70.Hart_en_AortaAsc',  'V75.Hart_en_AortaAsc',  'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
        'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV',  'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT43_6_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',# dosimetric PTV
        'VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen',  'DOSEMIN.Longen',  'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
        'VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'V70.Oes',  'V75.Oes',  'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
       )
data = data[, keep]

rm(drop, keep)

