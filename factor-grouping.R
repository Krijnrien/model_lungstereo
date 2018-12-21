library(caret)
library(survival)
library(survminer)
library(dplyr)
library(psych)

source("column-selection_stop-treatment.R")

data <- data %>% mutate(age_group = ifelse(agediag <=73, "age <= 73", "age > 73"))
data$cT_stat <- ifelse(data$cT_stat == 1 | data$cT_stat == 2 | data$cT_stat == 3, data$cN_rth, NA)
data <- data %>% mutate(age_group = ifelse(agediag <= 73, "FEV1 < 73", "FEV1 >= 73"))
data <- data %>% mutate(fev1_group = ifelse(FEV1 <= 70, "FEV1 < 70", "FEV1 >= 70"))
data$WHO <- ifelse(data$WHO == 3 , 2, data$WHO)
# 1 adenoca, 2 squamous cell carcinoma, 3 carconima NOS, 4 sarcoma ,5 BAC, 6 other malignancy, 9 no PA/ proved malignancy
data$PAstat <- ifelse(data$PA == 1 | data$PA == 2 | data$PA == 3 | data$PA == 4 | data$PA == 5 | data$PA == 6, 1, 2)
# 1 primary lung carcinoma, 2 metastasis lung carcinoma, 3 metastasis colon/ rectum, 4 metastasis sarcoma, 5 metastasis bladder, 6 metastasis prostate, 9 other metastasis
data$originstat[data$origin == 1] <- 1 # primary carcinoom
data$originstat[data$origin == 2] <- 2 # lung metastasen
data$originstat[data$origin == 3] <- 2
data$originstat[data$origin == 4] <- 2
data$originstat[data$origin == 5] <- 2
data$originstat[data$origin == 6] <- 2
data$originstat[data$origin == 9] <- 2



data$Schedule <- factor(data$Schedule, levels = c("1", "2", "3", "4"), labels = c("3x18Gy", "5x11Gy", "8x7,5Gy", "12x5Gy"))
#data$age_group <- factor(data$age_group)
data$cT_stat <- factor(data$cT_stat, levels = c("1", "2", "3"), labels = c("t1", "t2", "t3"))
data$WHO <- factor(data$WHO, levels = c("0", "1", "2", "4", "5"), labels = c("asymptomatic", "symptomatic but completely ambulatory", "symptomatic", "bedbound", "death")) # 0 asymptomatic, 1 symptomatic but completely ambulatory, 2 symptomatic, 3 symptomatic, 4 bedbound, 5 death
data$fev1_group <- factor(data$fev1_group)
data$age_group <- factor(data$age_group)
data$PAstat <- factor(data$PAstat, levels = c("1", "2"), labels = c("PA", "No PA")) # 1 adenoca, 2 squamous cell carcinoma, 3 carconima NOS, 4 sarcoma ,5 BAC, 6 other malignancy, 9 no PA/ proved malignancy
data$originstat <- factor(data$originstat, levels = c("1", "2"), labels = c("primair", "metastases"))
data$size <- factor(data$size)
data$aantal_doelgebieden <- factor(data$aantal_doelgebieden)
data$aantal_tumoren <- factor(data$aantal_tumoren)


