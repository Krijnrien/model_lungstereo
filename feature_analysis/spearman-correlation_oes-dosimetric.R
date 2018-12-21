library(caret)
library(survival)
library(survminer)
library(dplyr)
library(psych)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots

source("column-selection_stop-treatment.R") # Subset of all 302 patients who have complete Dicom data (no missing organ values, except 0 values)


Oes <- comb[, c("survivalstat", 'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes', 'D2CC.Oes',	'D2PRCT_INGY.Oes',	'D98PRCT_INGY.Oes',	'V95PRCT40_05_INPRCT.Oes',	'V95PRCT43_6_INPRCT.Oes',	'V95PRCT53_4_INPRCT.Oes',
                "V5.Oes", "V10.Oes", "V15.Oes", "V20.Oes", "V25.Oes", "V30.Oes", "V35.Oes", "V40.Oes", "V45.Oes", "V50.Oes", "V55.Oes", "V60.Oes")]
Oes[] <- lapply(Oes, function(x) as.numeric(as.character(x)))
Oes <- Oes %>% mutate_if(is.numeric, round, 0)

Oes_cor <- cor(Oes)

sink("output/lungstereo_oes_spearman-correlation-results.txt")
print(Oes_cor)
sink()

Oes_correlated_cutoff <- findCorrelation(Oes_cor, cutoff=0.75, names = TRUE)

sink("output/lungstereo_oes_dropped-columns.txt")
print(Oes_correlated_cutoff)
sink()

Oes_dropped = Oes[ , !(names(Oes) %in% Oes_correlated_cutoff)]

png('output/lungestereo_oes_spearman-all-pairspanels.png', width=3840, height=2160)
pairs.panels(Oes_cor, method = "spearman")
dev.off()

Oes_cor_subset <- cor(Oes[c("survivalstat", 'DOSEMIN.Oes', "V5.Oes", "V50.Oes")])
png('output/lungestereo_oes_spearman-selection-pairspanels.png', width=3840, height=2160)
pairs.panels(Oes_cor_subset, method = "spearman")
dev.off()

