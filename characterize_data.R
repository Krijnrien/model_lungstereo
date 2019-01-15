source("clean_data.r")

cat(sprintf("Total patients nr: %s\n", sum(length(unique(data$MRN)), na.rm = TRUE)))

cat("--------------\n")

cat(sprintf("Amount of males: %s\n", sum(data$sex == 1, na.rm = TRUE)))
cat(sprintf("Amount of females: %s\n", sum(data$sex == 2, na.rm = TRUE)))
cat(sprintf("unknown: %s\n", sum(is.na(data$sex))))

cat("--------------\n")

cat(sprintf("Diagnose age min: %s\n", min(data$agediag, na.rm = TRUE)))
cat(sprintf("Diagnose age median: %s\n", median(data$agediag, na.rm = TRUE)))
cat(sprintf("Diagnose age max: %s\n", max(data$agediag, na.rm = TRUE)))

cat("--------------\n")

survivalNA = sum(is.na(data$survival))
survivalNotDeath = sum(data$survival == 0, na.rm = TRUE)
survivalAliveWTumor = sum(data$survival == 1, na.rm = TRUE)
survivalDeathLocalFailure = sum(data$survival == 2, na.rm = TRUE)
survivalDeathLocoregionalFailure = sum(data$survival == 3, na.rm = TRUE)
survivalDeathMetastasis = sum(data$survival == 4, na.rm = TRUE)
survivalDeathLocoregionalAndMetastasis = sum(data$survival == 5, na.rm = TRUE)
survivalDeathSecondairWithoutPrimaryTumor = sum(data$survival == 6, na.rm = TRUE)
survivalDeathTherapyComplication = sum(data$survival == 7, na.rm = TRUE)
survivalDeathNotKnown = sum(data$survival == 8, na.rm = TRUE)
survivalDeathIntercurrentWithoutTumorActivity = sum(data$survival == 9, na.rm = TRUE)
survivalDeathIntercurrentWithTumorActivity = sum(data$survival == 10, na.rm = TRUE)

survivalTotalKnownDeath = survivalDeathLocalFailure + survivalDeathLocoregionalFailure + survivalDeathMetastasis + survivalDeathLocoregionalAndMetastasis + survivalDeathSecondairWithoutPrimaryTumor + survivalDeathTherapyComplication + survivalDeathNotKnown + survivalDeathIntercurrentWithoutTumorActivity + survivalDeathIntercurrentWithTumorActivity
SurvivalTotalKnownAlive = survivalNotDeath


cat(sprintf("Survival 0, not death: %s\n", survivalNotDeath))
cat(sprintf("Survival 1, alive with tumor: %s\n", survivalAliveWTumor))
cat(sprintf("Survival 2, death due to local failure: %s\n", survivalDeathLocalFailure))
cat(sprintf("Survival 3, death to locoregional failure: %s\n", survivalDeathLocoregionalFailure))
cat(sprintf("Survival 4, death due to metastasis: %s\n", survivalDeathMetastasis))
cat(sprintf("Survival 5, death to locoregional and metastatic failure: %s\n", survivalDeathLocoregionalAndMetastasis))
cat(sprintf("Survival 6, death secundaire tumor without primary tumor activity: %s\n", survivalDeathSecondairWithoutPrimaryTumor))
cat(sprintf("Survival 7, death due to complicaiton of therapy: %s\n", survivalDeathTherapyComplication))
cat(sprintf("Survival 8, death reason not known: %s\n", survivalDeathNotKnown))
cat(sprintf("Survival 9, death intercurrent without tumor activity: %s\n", survivalDeathIntercurrentWithoutTumorActivity))
cat(sprintf("Survival 10, death intercurrent with tumor activity: %s\n", survivalDeathIntercurrentWithTumorActivity))
cat(sprintf("unknown: %s\n", survivalNA))
cat("--------------\n")

cat(sprintf("Survival alive: %s\n", SurvivalTotalKnownAlive))
cat(sprintf("Survival alive with tumor: %s\n", survivalAliveWTumor))
cat(sprintf("Survival cancer related death: %s\n", survivalTotalKnownDeath))
cat(sprintf("Survival unknown: %s\n", survivalNA))

cat("--------------\n")

cat(sprintf("Survival total alive GBA: %s\n", sum(data$survivalstat == 0, na.rm = TRUE)))
cat(sprintf("Survival total dead gba: %s\n", sum(data$survivalstat == 1, na.rm = TRUE)))
cat(sprintf("Survival total unknown gba (should be 0): %s\n", sum(is.na(data$survivalstat))))
cat("--------------\n")



#3x18Gy", "5x11Gy", "8x7,5Gy

cat(sprintf("# 3x18Gy: %s\n", sum(data$Schedule == "1", na.rm = TRUE)))
cat(sprintf("# 5x11Gy: %s\n", sum(data$Schedule == "2", na.rm = TRUE)))
cat(sprintf("# 8x7Gy: %s\n", sum(data$Schedule == "3", na.rm = TRUE)))
cat(sprintf("# 12x5Gy: %s\n", sum(data$Schedule == 4, na.rm = TRUE)))
cat(sprintf("# unknown: %s\n", sum(is.na(data$Schedule))))

cat("--------------\n")

cat(sprintf("# WHO asymptomatic: %s\n", sum(data$WHO == 0, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic but completely ambulatory: %s\n", sum(data$WHO == 1, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic: %s\n", sum(data$WHO == 2, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic: %s\n", sum(data$WHO == 3, na.rm = TRUE)))
cat(sprintf("# WHO bedbound: %s\n", sum(data$WHO == 4, na.rm = TRUE)))
cat(sprintf("# WHO death: %s\n", sum(data$WHO == 5, na.rm = TRUE)))
cat(sprintf("# unknown: %s\n", sum(is.na(data$Schedule))))

cat("--------------\n")

cat(sprintf("# FEV < 70: %d \n", sum(as.numeric(data$FEV1) < 70, na.rm = TRUE)))
cat(sprintf("# FEV >= 70: %d \n", sum(as.numeric(data$FEV1) >= 70, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(data$FEV1))))

cat("--------------\n")

cat(sprintf("# T-stage 1: %d\n", sum(data$cT == "1", na.rm = TRUE)))
cat(sprintf("# T-stage 2: %d\n", sum(data$cT == "2", na.rm = TRUE)))
cat(sprintf("# T-stage 3: %d\n", sum(data$cT == "3", na.rm = TRUE)))
cat(sprintf("# T-stage unknown: %d\n", sum(is.na(data$cT))))

cat("--------------\n")

cat(sprintf("# n-stage 1: %d\n", sum(data$cN == "1", na.rm = TRUE)))
cat(sprintf("# n-stage 2: %d\n", sum(data$cN == "2", na.rm = TRUE)))
cat(sprintf("# n-stage 3: %d\n", sum(data$cN == "3", na.rm = TRUE)))
cat(sprintf("# n-stage unknown: %d\n", sum(is.na(data$cN) | data$cN == "")))

cat("--------------\n")

data$size <- as.numeric(as.character(data$size))
cat(sprintf("tumor diameter min: %s\n", min(data$size, na.rm = TRUE)))
cat(sprintf("tumor diameter  median: %s\n", median(data$size, na.rm = TRUE)))
cat(sprintf("tumor diameter  max: %s\n", max(data$size, na.rm = TRUE)))

cat("--------------\n")

cat(sprintf("# location LBK: %d\n", sum(data$location == 1, na.rm = TRUE)))
cat(sprintf("# location LOK: %d\n", sum(data$location == 2, na.rm = TRUE)))
cat(sprintf("# location RBK: %d\n", sum(data$location == 3, na.rm = TRUE)))
cat(sprintf("# location RMK: %d\n", sum(data$location == 4, na.rm = TRUE)))
cat(sprintf("# location ROK: %d\n", sum(data$location == 5, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(data$location))))

cat("--------------\n")

cat(sprintf("# location bovenkwab: %d\n", sum(data$location_stat == 1, na.rm = TRUE)))
cat(sprintf("# location middenkwab: %d\n", sum(data$location_stat == 3, na.rm = TRUE)))
cat(sprintf("# location onderkwab: %d\n", sum(data$location_stat == 2, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(data$location_stat))))

cat(sprintf("# metastized no survival mean: %d\n", sum(data$cM == 0, na.rm = TRUE)))
cat(sprintf("# metastized yes survival mean: %d\n", sum(data$cM == 1, na.rm = TRUE)))

cat("Average survival status per cM")
aggregate( survivalstat ~ cM, data, mean )
