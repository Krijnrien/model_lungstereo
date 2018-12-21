library(caret)
library(survival)
library(survminer)
library(dplyr)
library(psych)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots

source("column-selection_stop-treatment.R") # Subset of all 302 patients who have complete Dicom data (no missing organ values, except 0 values)


Longen <- comb[, c("survivalstat", 'DOSEMEAN.Longen',  'DOSEMAX.Longen', 'DOSESTD.Longen', 'D2CC.Longen',	'D2PRCT_INGY.Longen',	'D98PRCT_INGY.Longen',	'V95PRCT40_05_INPRCT.Longen',	'V95PRCT43_6_INPRCT.Longen',	'V95PRCT53_4_INPRCT.Longen',
                   "V5.Longen", "V10.Longen", "V15.Longen", "V20.Longen", "V25.Longen", "V30.Longen", "V35.Longen", "V40.Longen", "V45.Longen", "V50.Longen", "V55.Longen", "V60.Longen", "V65.Longen", "V70.Longen", "V75.Longen")]
Longen[] <- lapply(Longen, function(x) as.numeric(as.character(x)))
Longen <- Longen %>% mutate_if(is.numeric, round, 0)

longen_cor <- cor(Longen)

sink("output/lungstereo_Longen_spearman-correlation-results.txt")
print(longen_cor)
sink()

longen_correlated_cutoff <- findCorrelation(longen_cor, cutoff=0.75, names = TRUE)

sink("output/lungstereo_Longen_dropped-columns.txt")
print(longen_correlated_cutoff)
sink()

longen_dropped = Longen[ , !(names(Longen) %in% longen_correlated_cutoff)]

png('output/lungestereo_Longen_spearman-all-pairspanels.png', width=3840, height=2160)
pairs.panels(longen_cor, method = "spearman")
dev.off()

Longen_cor_subset <- cor(Longen[c("survivalstat", 'DOSEMAX.Longen',  'DOSESTD.Longen', 'D98PRCT_INGY.Longen',	
                                  "V5.Longen", "V70.Longen", "V75.Longen")])
png('output/lungestereo_Longen_spearman-selection-pairspanels.png', width=3840, height=2160)
pairs.panels(Longen_cor_subset, method = "spearman")
dev.off()
