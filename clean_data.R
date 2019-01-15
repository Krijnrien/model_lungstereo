library(dplyr)

source("load_data.r")

# 1 = upper lobe, 2 = lower lobe, 3 is middle lobe
data["location_stat"] <- NA

# 1 LBK, 2 LOK, 3 RBK, 4 RMK, 5 ROK
data$location_stat[data$location == 1] <- 1
data$location_stat[data$location == 3] <- 1
data$location_stat[data$location == 2] <- 2
data$location_stat[data$location == 5] <- 2
data$location_stat[data$location == 4] <- 3



# Fill missing castor location with RTH location (which only has 3 options (missing left/right) compared to 5 castor locations)
data_NA_location <- data[which(is.na(data$location)),]
data_location <- data[which(!is.na(data$location)),]

data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 303] <- 1
data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 305] <- 2
data_NA_location$location_stat[data_NA_location$code_localisatie_doelgebied == 304] <- 3

data <- rbind(data_NA_location, data_location)
rm(data_NA_location, data_location)



# Fill missing castor SBRT schedule with schedule from RTH
data_NAschedule <- data[which(is.na(data$Schedule)),]
data_schedule <- data[which(!is.na(data$Schedule)),]

data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 3] <- 1
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 5] <- 2
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 8] <- 3
data_NAschedule$Schedule[data_NAschedule$cum_aantal_fracties == 12] <- 4

data <- rbind(data_schedule, data_NAschedule)
rm(data_NAschedule, data_schedule)



# # Fill missing castor cN value with RTH extracted cT
# data$cT <- ifelse(is.na(data$cT), data$cT_rth, data$cT)
# # Fill missing castor cN value with RTH extracted cN
# data$cN <- ifelse(is.na(data$cN), data$cN_rth, data$cN)
# # Fill missing castor cM value with RTH extracted cM
# data$cM <- ifelse(is.na(data$cM), data$cM_rth, data$cM)
# 
# # Only take first character (aka removing A and B notation from tnm-stage)
# data$cT_stat <- substr(data$cT, 0, 1)
# data$cN_stat <- substr(data$cN, 0, 1)
# data$cM_stat <- substr(data$cM, 0, 1)


data$cT <- ifelse(is.na(data$cT), data$cT_rth, data$cT)
data$cN <- ifelse(is.na(data$cN), data$cN_rth, data$cN)
data$cM <- ifelse(is.na(data$cM), data$cM_rth, data$cM)

#data$cT <- ifelse(data$cT != 1 | data$cT != 2 | data$cT != 3, NA, data$cT)
#data$cN <- ifelse(data$cN != 0 | data$cN != 1 | data$cN != 2 | data$cN != 3, 0, data$cN)
data$cN <- ifelse(data$cN == "x", 0, data$cN)
#data$cM <- ifelse(data$cM != 0 | data$cM != 1, NA, data$cM)

data$cT_stat <- substr(data$cT, 0, 1)
data$cN_stat <- substr(data$cN, 0, 1)
data$cM_stat <- substr(data$cM, 0, 1)





# Create binary survival value. If datum_overlijden_vlg_gba value is 2018-10-30 (date of export of GBA) then patient is still alive. Thus 0 (event didnt happen) otherwise make 1 (event happened)
data$survivalstat <- ifelse(data$datum_overlijden_vlg_gba == "2018-10-30", 0, 1)


# Calculate amount of days between death and finish treatment
data$diff_in_days <- difftime(data$datum_overlijden_vlg_gba, data$einddatum_cum_dosis, units = c("days"))


# list of all columns that should be numerical
toNumericals = c('survivalstat', 'cN_stat','aantal_doelgebieden','FEV1', 'size','cum_dosis_specificatie', 'agediag', 'diff_in_days', #'cT_stat', 'cN_stat', 'cM_stat',
                 'VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc',  'V65.Hart_en_AortaAsc',  'V70.Hart_en_AortaAsc',  'V75.Hart_en_AortaAsc',  'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc',
                 'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV',  'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT43_6_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',
                 'VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen',  'DOSEMIN.Longen',  'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen',
                 'VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'V70.Oes',  'V75.Oes',  'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'
)
# convert columns to numerical
data[,toNumericals] <- lapply(data[, toNumericals], function(x) as.numeric(as.character(x)))
# Round all numericals, 0 decimal places
data <- data %>% mutate_if(is.numeric, round, 0)

rm(toNumericals)
