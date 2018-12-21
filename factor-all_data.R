source("clean.R")


# list of all columns that should be factors
factorCols = c('sex', 'WHO', 'PrePneumonitis',	'PreHoest',	'PreDyspnoe',	'PreAnorexia',	'PreOesophagitis',	'PreDerrmatitis',	'PreOsteoradionecrose', 
               'PA', 'origin', 'Schedule',
               
               'Week1Hoest', 'Week1Dyspnoe',	'Week1Anorexia',	'Week1Oesophagitis',	'Week1Dermatitis',	'Week1Osteoradionecrose',	'Week1Pneumonitis',	
               'Week2Hoest',	'Week2Dyspnoe',	'Week2Anorexia',	'Week2Oesophagitis',	'Week2Dermatitis',	'Week2Osteoradionecrose',	'Week2Pneumonitis',	
               'Week3Hoest',	'Week3Dyspnoe',	'Week3Anorexia',	'Week3Oesophagitis',	'Week3Dermatitis',	'Week3Osteoradionecrose',	'Week3Pneumonitis',	
               'Mnd1Pneumonitis',	'Mnd1Hoest',	'Mnd1Dyspnoe',	'Mnd1Anorexia',	'Mnd1Oesophagitis',	'Mnd1Dermatitis',	'Mnd1Osteoradionecrose',
               'Mnd6Pneumonitis',	'Mnd6Hoest',	'Mnd6Dyspnoe',	'Mnd6Anorexia',	'Mnd6Oesophagitis',	'Mnd6Dermatitis',	'Mnd6Osteoradionecrose',
               'Mnd12Pneumonitis',	'Mnd12Hoest',	'Mnd12Dyspnoe',	'Mnd12Anorexia',	'Mnd12Oesophagitis',	'Mnd12Dermatitis',	'Mnd12Osteoradionecrose',
               'Mnd24Pneumonitis',	'Mnd24Hoest',	'Mnd24Dyspnoe',	'Mnd24Anorexia',	'Mnd24Oesophagitis',	'Mnd24Dermatitis',	'Mnd24Osteoradionecrose',
               
               'aantal_tumoren',	'aantal_doelgebieden',	'volgnummer_tumor',	'volgnummer_doelgebied',	'volgnummer_radiotherapie',
               'code_localisatie_doelgebied', 'opzet_radiotherapie',	'indicatie_bestraling', 'location_stat', 'cT_stat', 'cN_stat', 'cM_stat', 'survivalstat'
)
# convert columns into factor variables
data[factorCols] = lapply(data[factorCols], factor) 


