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


# All Longen variables (except no variance)
all_keep = c('VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
             'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV',  'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT43_6_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',# dosimetric PTV
             'VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen', 'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
             'VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
)
all_data = data[, all_keep]


all_data[] <- lapply(all_data, function(x) as.numeric(as.character(x)))
all_data <- all_data %>% mutate_if(is.numeric, round, 0)



all_data_cor <- cor(all_data)
sink("output/lungstereo_all_spearman-correlation-results.txt")
print(all_data_cor)
sink()

all_data_correlated_cutoff <- findCorrelation(all_data_cor, cutoff=0.90, names = TRUE)

sink("output/lungstereo_all_dropped-columns.txt")
print(all_data_correlated_cutoff)
sink()

all_dropped = all_data[ , !(names(all_data) %in% all_data_correlated_cutoff)]

png('output/lungestereo_all_spearman-all-pairspanels.png', width=15360, height=8640)
pairs.panels(all_data_cor, method = "spearman")
dev.off()








subset_keep = c('V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
             'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',# dosimetric PTV
             'VOLUME.Longen', 'DOSEMAX.Longen', 'D98PRCT_INGY.Longen', #dosimetric Lungs
             'DOSEMEAN.Oes',  'DOSEMAX.Oes', 'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
)
subset_data = data[, subset_keep]

data_subset_cor <- cor(subset_data)
png('output/lungestereo_all_spearman-selection-pairspanels.png', width=7680, height=4320)
pairs.panels(data_subset_cor, method = "spearman")
dev.off()

