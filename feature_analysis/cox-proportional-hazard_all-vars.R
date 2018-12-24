library(survival)
library(survminer)
library(dplyr)
library(ggfortify)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off() #clear graphics

source("factor-grouping.R")



surv_object <- Surv(data$diff_in_days, data$survivalstat)



sink("output/lungstereo_castor-rth_univariate-cox-regression_output-table.csv")
cat("var,Rsquare,Likelihood ratio test,Wald test,Score (logrank) test\n")

#univariate cox regression on grouping fev < 70 and fev > 70
fev1_group_cox <- coxph(surv_object ~ fev1_group, data = data)
summary(fev1_group_cox)
ggforest(fev1_group_cox, data = data)

summcph <- summary(fev1_group_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on fev numerical
fev1_cox <- coxph(surv_object ~ FEV1, data = data)
summary(fev1_cox)
ggforest(fev1_cox, data = data)

summcph <- summary(fev1_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on WHO status
who_cox <- coxph(surv_object ~ WHO, data = data)
summary(who_cox)
ggforest(who_cox, data = data)

summcph <- summary(who_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on PA proven
pa_cox <- coxph(surv_object ~ PAstat, data = data)
summary(pa_cox)
ggforest(pa_cox, data = data)

summcph <- summary(pa_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")

#univariate cox regression on SBRT schedule
schedule_cox <- coxph(surv_object ~ Schedule, data = data)
summary(schedule_cox)
ggforest(schedule_cox, data = data)

summcph <- summary(schedule_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on originstat (primary & metastasis)
origin_cox <- coxph(surv_object ~ originstat, data = data)
summary(origin_cox)
ggforest(origin_cox, data = data)

summcph <- summary(origin_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on T-stage
ct_cox <- coxph(surv_object ~ cT_stat, data = data)
summary(ct_cox)
ggforest(ct_cox, data = data)

summcph <- summary(ct_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


#univariate cox regression on cN
cN_cox <- coxph(surv_object ~ cN_stat, data = data)
summary(cN_cox)
ggforest(cN_cox, data = data)

summcph <- summary(cN_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


#univariate cox regression on cM
cM_cox <- coxph(surv_object ~ cM_stat, data = data)
summary(cM_cox)
ggforest(cM_cox, data = data)

summcph <- summary(cM_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on tumor size
tumor_size_cox <- coxph(surv_object ~ size, data = data)
summary(tumor_size_cox)
ggforest(tumor_size_cox, data = data)

summcph <- summary(tumor_size_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


#univariate cox regression on 'amount of target areas'
amount_targets_cox <- coxph(surv_object ~ aantal_doelgebieden, data = data)
summary(amount_targets_cox)
ggforest(amount_targets_cox, data = data)

summcph <- summary(amount_targets_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


#univariate cox regression on gender
gender_cox <- coxph(surv_object ~ sex, data = data)
summary(gender_cox)
ggforest(gender_cox, data = data)

summcph <- summary(gender_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on age
agediag_cox <- coxph(surv_object ~ agediag, data = data)
summary(agediag_cox)
ggforest(agediag_cox, data = data)

summcph <- summary(agediag_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")



#univariate cox regression on age group
age_group_cox <- coxph(surv_object ~ age_group, data = data)
summary(age_group_cox)
ggforest(age_group_cox, data = data)

summcph <- summary(age_group_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


#univariate cox regression on age group
ptv75_cox <- coxph(surv_object ~ V5.Hart_en_AortaAsc, data = data)
summary(ptv75_cox)
ggforest(ptv75_cox, data = data)

summcph <- summary(ptv75_cox)
cat(summcph$call$formula[[3]], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")


sink()


# univariate analysis on ALL dosimetric parameters

#IF YOU GET A 'subscript out of bounds' error. IT MEANS THAT ONE OF YOUR COLUMNS IS WRONG, LIKELY NA.
dosimetric_list <- c('DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV', 'D2CC.PTV',	'D2PRCT_INGY.PTV',	'D98PRCT_INGY.PTV',	'V95PRCT40_05_INPRCT.PTV',	'V95PRCT43_6_INPRCT.PTV',	'V95PRCT53_4_INPRCT.PTV', "V5.PTV", "V10.PTV", "V15.PTV", "V20.PTV", "V25.PTV", "V30.PTV", "V35.PTV", "V40.PTV", "V45.PTV", "V50.PTV", "V55.PTV", "V60.PTV", "V65.PTV", "V70.PTV", "V75.PTV",
                     'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',	'D2PRCT_INGY.Hart_en_AortaAsc',	'D98PRCT_INGY.Hart_en_AortaAsc',	'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',	'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',	'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', "V5.Hart_en_AortaAsc", "V10.Hart_en_AortaAsc", "V15.Hart_en_AortaAsc", "V20.Hart_en_AortaAsc", "V25.Hart_en_AortaAsc", "V30.Hart_en_AortaAsc", "V35.Hart_en_AortaAsc", "V40.Hart_en_AortaAsc", "V45.Hart_en_AortaAsc", "V50.Hart_en_AortaAsc", "V55.Hart_en_AortaAsc", "V60.Hart_en_AortaAsc",
                     'DOSEMEAN.Longen',  'DOSEMAX.Longen', 'DOSESTD.Longen', 'D2CC.Longen',	'D2PRCT_INGY.Longen',	'D98PRCT_INGY.Longen',	'V95PRCT40_05_INPRCT.Longen',	'V95PRCT43_6_INPRCT.Longen',	'V95PRCT53_4_INPRCT.Longen', "V5.Longen", "V10.Longen", "V15.Longen", "V20.Longen", "V25.Longen", "V30.Longen", "V35.Longen", "V40.Longen", "V45.Longen", "V50.Longen", "V55.Longen", "V60.Longen", "V65.Longen", "V70.Longen", "V75.Longen",
                     'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes', 'D2CC.Oes',	'D2PRCT_INGY.Oes',	'D98PRCT_INGY.Oes',	'V95PRCT40_05_INPRCT.Oes',	'V95PRCT43_6_INPRCT.Oes',	'V95PRCT53_4_INPRCT.Oes', "V5.Oes", "V10.Oes", "V15.Oes", "V20.Oes", "V25.Oes", "V30.Oes", "V35.Oes", "V40.Oes", "V45.Oes", "V50.Oes", "V55.Oes", "V60.Oes"
                     )

result <- list()

for (i in 1:length(dosimetric_list)){
  print(temp_cox <- coxph(surv_object ~ get(dosimetric_list[i]), data=data))
  print(result[[i]] <- temp_cox)
}


sink("output/lungstereo_dicom_univariate-cox-regression_output-table.csv")
cat("var,Rsquare,Likelihood ratio test,Wald test,Score (logrank) test\n")

for (i in 1:length(result)){
  summcph <- summary(result[[i]])
  cat(dosimetric_list[i], ",", summcph$rsq[[1]], ",", summcph$logtest[[3]], ",", summcph$waldtest[[3]], ",", summcph$sctest[[3]],"\n")
}

sink()

