library(caret)
library(survival)
library(survminer)
library(dplyr)
library(psych)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots

source("column-selection_stop-treatment.R") # Subset of all 302 patients who have complete Dicom data (no missing organ values, except 0 values)


ptv <- comb[, c("survivalstat", 'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV', 'D2CC.PTV',	'D2PRCT_INGY.PTV',	'D98PRCT_INGY.PTV',	'V95PRCT40_05_INPRCT.PTV',	'V95PRCT43_6_INPRCT.PTV',	'V95PRCT53_4_INPRCT.PTV',
                "V5.PTV", "V10.PTV", "V15.PTV", "V20.PTV", "V25.PTV", "V30.PTV", "V35.PTV", "V40.PTV", "V45.PTV", "V50.PTV", "V55.PTV", "V60.PTV", "V65.PTV", "V70.PTV", "V75.PTV")]
ptv[] <- lapply(ptv, function(x) as.numeric(as.character(x)))
ptv <- ptv %>% mutate_if(is.numeric, round, 0)

sink("output/lungstereo_ptv_spearman-correlation-results.txt")
ptv_cor <- cor(ptv)

sink()


sink("output/lungstereo_ptv_dropped-columns.txt")
print(ptv_cor)
sink()


ptv_correlated_cutoff <- findCorrelation(ptv_cor, cutoff=0.75, names = TRUE)
print(ptv_correlated_cutoff)
#ptv_dropped = ptv[ , !(names(ptv) %in% ptv_correlated_cutoff)]

png('output/lungestereo_ptv_spearman-all-pairspanels.png', width=3840, height=2160)
pairs.panels(ptv_cor, method = "spearman")
dev.off()

ptv_cor_subset <- cor(ptv[c("survivalstat", 'DOSEMIN.PTV',  'DOSESTD.PTV', "V5.PTV", "V75.PTV")])
png('output/lungestereo_ptv_spearman-selection-pairspanels.png', width=3840, height=2160)
pairs.panels(ptv_cor_subset, method = "spearman")
dev.off()