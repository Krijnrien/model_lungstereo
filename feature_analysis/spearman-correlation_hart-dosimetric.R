library(caret)
library(survival)
library(survminer)
library(dplyr)
library(psych)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots

source("column-selection_stop-treatment.R") # Subset of all 302 patients who have complete Dicom data (no missing organ values, except 0 values)
# preprocess functions
source('impute_data.r')


zero_variance_columns = remove_zero_variance_columns(data)
varied_data <- zero_variance_columns
data <- impute_data(varied_data)


Hart_en_AortaAsc <- data[, c("survivalstat", 'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',	'D2PRCT_INGY.Hart_en_AortaAsc',	'D98PRCT_INGY.Hart_en_AortaAsc',	'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',	'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',	'V95PRCT53_4_INPRCT.Hart_en_AortaAsc',
                             "V5.Hart_en_AortaAsc", "V10.Hart_en_AortaAsc", "V15.Hart_en_AortaAsc", "V20.Hart_en_AortaAsc", "V25.Hart_en_AortaAsc", "V30.Hart_en_AortaAsc", "V35.Hart_en_AortaAsc", "V40.Hart_en_AortaAsc", "V45.Hart_en_AortaAsc", "V50.Hart_en_AortaAsc", "V55.Hart_en_AortaAsc", "V60.Hart_en_AortaAsc")]
Hart_en_AortaAsc[] <- lapply(Hart_en_AortaAsc, function(x) as.numeric(as.character(x)))
Hart_en_AortaAsc <- Hart_en_AortaAsc %>% mutate_if(is.numeric, round, 0)

Hart_en_AortaAsc_cor <- cor(Hart_en_AortaAsc)
sink("output/lungstereo_hart_spearman-correlation-results.txt")
print(Hart_en_AortaAsc_cor)
sink()

Hart_en_AortaAsc_correlated_cutoff <- findCorrelation(Hart_en_AortaAsc_cor, cutoff=0.75, names = TRUE)

sink("output/lungstereo_hart_dropped-columns.txt")
print(Hart_en_AortaAsc_correlated_cutoff)
sink()

Hart_en_AortaAsc_dropped = Hart_en_AortaAsc[ , !(names(Hart_en_AortaAsc) %in% Hart_en_AortaAsc_correlated_cutoff)]

png('output/lungestereo_hart_spearman-all-pairspanels.png', width=3840, height=2160)
pairs.panels(Hart_en_AortaAsc_cor, method = "spearman")
dev.off()

Hart_en_AortaAsc_cor_subset <- cor(Hart_en_AortaAsc[c("survivalstat", 'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc', 'D98PRCT_INGY.Hart_en_AortaAsc', "V5.Hart_en_AortaAsc", "V60.Hart_en_AortaAsc")])
png('output/lungestereo_hart_spearman-selection-pairspanels.png', width=3840, height=2160)
pairs.panels(Hart_en_AortaAsc_cor_subset, method = "spearman")
dev.off()

