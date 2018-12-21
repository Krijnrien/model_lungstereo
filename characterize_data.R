if(!exists("comb")){
  cat("No comb object found in memory: running prepare.r script!")
  source("clean.r")
}

cat(sprintf("Total patients nr: %s\n", sum(length(unique(comb$MRN)), na.rm = TRUE)))

cat("--------------\n")

cat(sprintf("Amount of males: %s\n", sum(comb$sex == 1, na.rm = TRUE)))
cat(sprintf("Amount of females: %s\n", sum(comb$sex == 2, na.rm = TRUE)))
cat(sprintf("unknown: %s\n", sum(is.na(comb$sex))))

cat("--------------\n")

cat(sprintf("Diagnose age min: %s\n", min(comb$agediag, na.rm = TRUE)))
cat(sprintf("Diagnose age median: %s\n", median(comb$agediag, na.rm = TRUE)))
cat(sprintf("Diagnose age max: %s\n", max(comb$agediag, na.rm = TRUE)))

cat("--------------\n")

survivalNA = sum(is.na(comb$survival))
survivalNotDeath = sum(comb$survival == 0, na.rm = TRUE)
survivalAliveWTumor = sum(comb$survival == 1, na.rm = TRUE)
survivalDeathLocalFailure = sum(comb$survival == 2, na.rm = TRUE)
survivalDeathLocoregionalFailure = sum(comb$survival == 3, na.rm = TRUE)
survivalDeathMetastasis = sum(comb$survival == 4, na.rm = TRUE)
survivalDeathLocoregionalAndMetastasis = sum(comb$survival == 5, na.rm = TRUE)
survivalDeathSecondairWithoutPrimaryTumor = sum(comb$survival == 6, na.rm = TRUE)
survivalDeathTherapyComplication = sum(comb$survival == 7, na.rm = TRUE)
survivalDeathNotKnown = sum(comb$survival == 8, na.rm = TRUE)
survivalDeathIntercurrentWithoutTumorActivity = sum(comb$survival == 9, na.rm = TRUE)
survivalDeathIntercurrentWithTumorActivity = sum(comb$survival == 10, na.rm = TRUE)

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

cat(sprintf("Survival total alive GBA: %s\n", sum(comb$survivalstat == 0, na.rm = TRUE)))
cat(sprintf("Survival total dead gba: %s\n", sum(comb$survivalstat == 1, na.rm = TRUE)))
cat(sprintf("Survival total unknown gba (should be 0): %s\n", sum(is.na(comb$survivalstat))))
cat("--------------\n")



#3x18Gy", "5x11Gy", "8x7,5Gy

cat(sprintf("# 3x18Gy: %s\n", sum(comb$Schedule == "1", na.rm = TRUE)))
cat(sprintf("# 5x11Gy: %s\n", sum(comb$Schedule == "2", na.rm = TRUE)))
cat(sprintf("# 8x7Gy: %s\n", sum(comb$Schedule == "3", na.rm = TRUE)))
cat(sprintf("# 12x5Gy: %s\n", sum(comb$Schedule == 4, na.rm = TRUE)))
cat(sprintf("# unknown: %s\n", sum(is.na(comb$Schedule))))

cat("--------------\n")

cat(sprintf("# WHO asymptomatic: %s\n", sum(comb$WHO == 0, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic but completely ambulatory: %s\n", sum(comb$WHO == 1, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic: %s\n", sum(comb$WHO == 2, na.rm = TRUE)))
cat(sprintf("# WHO symptomatic: %s\n", sum(comb$WHO == 3, na.rm = TRUE)))
cat(sprintf("# WHO bedbound: %s\n", sum(comb$WHO == 4, na.rm = TRUE)))
cat(sprintf("# WHO death: %s\n", sum(comb$WHO == 5, na.rm = TRUE)))
cat(sprintf("# unknown: %s\n", sum(is.na(comb$Schedule))))

cat("--------------\n")

cat(sprintf("# FEV < 70: %d \n", sum(as.numeric(comb$FEV1) < 70, na.rm = TRUE)))
cat(sprintf("# FEV >= 70: %d \n", sum(as.numeric(comb$FEV1) >= 70, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(comb$FEV1))))

cat("--------------\n")

cat(sprintf("# T-stage 1: %d\n", sum(comb$cT == "1", na.rm = TRUE)))
cat(sprintf("# T-stage 2: %d\n", sum(comb$cT == "2", na.rm = TRUE)))
cat(sprintf("# T-stage 3: %d\n", sum(comb$cT == "3", na.rm = TRUE)))
cat(sprintf("# T-stage unknown: %d\n", sum(is.na(comb$cT))))

cat("--------------\n")

cat(sprintf("# n-stage 1: %d\n", sum(comb$cN == "1", na.rm = TRUE)))
cat(sprintf("# n-stage 2: %d\n", sum(comb$cN == "2", na.rm = TRUE)))
cat(sprintf("# n-stage 3: %d\n", sum(comb$cN == "3", na.rm = TRUE)))
cat(sprintf("# n-stage unknown: %d\n", sum(is.na(comb$cN) | comb$cN == "")))

cat("--------------\n")

comb$size <- as.numeric(as.character(comb$size))
cat(sprintf("tumor diameter min: %s\n", min(comb$size, na.rm = TRUE)))
cat(sprintf("tumor diameter  median: %s\n", median(comb$size, na.rm = TRUE)))
cat(sprintf("tumor diameter  max: %s\n", max(comb$size, na.rm = TRUE)))

cat("--------------\n")

cat(sprintf("# location LBK: %d\n", sum(comb$location == 1, na.rm = TRUE)))
cat(sprintf("# location LOK: %d\n", sum(comb$location == 2, na.rm = TRUE)))
cat(sprintf("# location RBK: %d\n", sum(comb$location == 3, na.rm = TRUE)))
cat(sprintf("# location RMK: %d\n", sum(comb$location == 4, na.rm = TRUE)))
cat(sprintf("# location ROK: %d\n", sum(comb$location == 5, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(comb$location))))

cat("--------------\n")

cat(sprintf("# location bovenkwab: %d\n", sum(comb$location_stat == 1, na.rm = TRUE)))
cat(sprintf("# location middenkwab: %d\n", sum(comb$location_stat == 3, na.rm = TRUE)))
cat(sprintf("# location onderkwab: %d\n", sum(comb$location_stat == 2, na.rm = TRUE)))
cat(sprintf("# unknown: %d\n", sum(is.na(comb$location_stat))))

cat(sprintf("# metastized no survival mean: %d\n", sum(comb$cM == 0, na.rm = TRUE)))
cat(sprintf("# metastized yes survival mean: %d\n", sum(comb$cM == 1, na.rm = TRUE)))

cat("Average survival status per cM")
aggregate( survivalstat ~ cM, comb, mean )
