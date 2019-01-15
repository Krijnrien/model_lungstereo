library(survival)
library(survminer)
library(dplyr)
library(ggfortify)
library(visdat)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off() #clear graphics

source("column-selection_stop-treatment.R")
source("impute_data.r")

zero_variance_columns = remove_zero_variance_columns(data)
varied_data <- zero_variance_columns
data <- impute_data(varied_data)
rm(varied_data, zero_variance_columns)

#data$survivalstat <- ifelse(data$diff_in_days > 730, 0, data$survivalstat)

# list of all columns that should be numerical
toNumericals = c('survivalstat', 'cN_stat')
# convert columns to numerical
data[,toNumericals] <- lapply(data[, toNumericals], function(x) as.numeric(as.character(x)))
# Round all numericals, 0 decimal places
data <- data %>% mutate_if(is.numeric, round, 0)


data <- data %>% mutate(age_group = ifelse(agediag <=73, "age <= 73", "age > 73"))
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



#data$survivalstat <- factor(data$survivalstat, levels = c(0, 1))
data$Schedule <- factor(data$Schedule, levels = c("1", "2", "3", "4"), labels = c("3x18Gy", "5x11Gy", "8x7,5Gy", "12x5Gy"))
#data$age_group <- factor(data$age_group)
data$cT_stat <- factor(data$cT_stat, levels = c("1", "2", "3"), labels = c("t1", "t2", "t3"))
#data$cN_stat <- factor(data$cN_stat, levels = c("0", "1", "2", "3"), labels = c("n0", "n1", "n2", "n3"))

#data$cN_stat <- factor(data$cN_stat)


data$cM_stat <- factor(data$cM_stat, levels = c("0", "1"), labels = c("m0", "m1"))
data$WHO <- factor(data$WHO, levels = c("0", "1", "2"), labels = c("asymptomatic", "symptomatic but completely ambulatory", "symptomatic")) # 0 asymptomatic, 1 symptomatic but completely ambulatory, 2 symptomatic, 3 symptomatic, 4 bedbound, 5 death
data$fev1_group <- factor(data$fev1_group)
data$age_group <- factor(data$age_group)
data$PAstat <- factor(data$PAstat, levels = c("1", "2"), labels = c("PA", "No PA")) # 1 adenoca, 2 squamous cell carcinoma, 3 carconima NOS, 4 sarcoma ,5 BAC, 6 other malignancy, 9 no PA/ proved malignancy
data$originstat <- factor(data$originstat, levels = c("1", "2"), labels = c("primair", "metastases"))
data$sex <- factor(data$sex, levels = c("1", "2"), labels = c("male", "female"))
#data$aantal_doelgebieden <- factor(data$aantal_doelgebieden)
data$aantal_tumoren <- factor(data$aantal_tumoren)






surv_object <- Surv(data$diff_in_days, data$survivalstat)

# smp_size <- floor(0.8 * nrow(data))
# ## set the seed to make partition reproducible
# train_ind <- sample(seq_len(nrow(data)), size = smp_size)
# train <- data[train_ind, ]
# test <- data[-train_ind, ]
# 
# 
# 
# surv_object <- Surv(train$diff_in_days, train$survivalstat)
data$cN_stat <- factor(data$cN_stat)


# Columns to keep
keep = c('survivalstat', 'cN_stat')
data = data[, keep]



vis_dat(data)

#multivariate CASTOR cox regression
#multi_cox <- coxph(surv_object ~ WHO + sex + cN_stat, data = data)
#multi_cox <- coxph(surv_object ~ WHO + sex + aantal_doelgebieden, data = data)
multi_cox <- coxph(surv_object ~ cN_stat, data = data, control = coxph.control(iter.max = 50))

summary(multi_cox)
ggforest(multi_cox, data = data)

summcph <- summary(multi_cox)
#cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")






# 
# cox_model = coxph(surv_object ~ WHO + sex + age_group, data=train)
# sum.surv <- summary(cox_model)
# ggforest(cox_model, data = data)
# c_index <- sum.surv$concordance
# c_index
# 
# 
# #multivariate cox regression
# multi_cox <- coxph(surv_object ~ WHO + age_group + originstat, data = train)
# summary(multi_cox)
# ggforest(multi_cox, data = data)
# 
# summcph <- summary(multi_cox)
# #cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")
# 
# 
# 
