#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")

library(imputeMissings)
library(ggbiplot)
library(naniar)
library(visdat)

cat("\014") # clear the console
rm(list = ls()) # clear workspace/environment (all variables and functions)
graphics.off() # clear current plots

if(!exists("comb")){
  cat("No comb object found in memory: running prepare.r script!\n")
  source("ML-preparation_stop-treatment.r")
}

# preprocess functions
source('ML_process.r')


#PCA only accept numerical values
#data.survival <- data[,c("survivalstat")]
# keep = c('FEV1', 'size', 'agediag', 'diff_in_days','cum_dosis_specificatie',
#         'VOLUME.Hart_en_AortaAsc', 'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc',  'V65.Hart_en_AortaAsc',  'V70.Hart_en_AortaAsc',  'V75.Hart_en_AortaAsc',  'D2CC.Hart_en_AortaAsc', #dosimetric heart and aorta
#         'VOLUME.PTV', 'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV', # dosimetric PTV
#         'VOLUME.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen', #dosimetric Lungs
#         'VOLUME.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'V70.Oes',  'V75.Oes',  'D2CC.Oes',  'D98PRCT_INGY.Oes'#dosimetric oesophagus
# )
# data = data[, keep]

#PCA only accept numerical values
# data.survival <- data[,c("survivalstat")]
# keep = c('VOLUME.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'V70.Oes',  'V75.Oes',  'D2CC.Oes',  'D98PRCT_INGY.Oes'#dosimetric oesophagus
# )
# data = data[, keep]

# data.survival <- data[,c("survivalstat")]
# keep = c('DOSEMEAN.Longen',  'DOSEMAX.Longen',  'DOSEMIN.Longen',  'DOSESTD.Longen', 'VOLUME.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen')
# data = data[, keep]


# PTV GOOD!!!!!!
# data.survival <- data[,c("survivalstat")]
# keep = c('DOSESTD.PTV', "V60.PTV", "V65.PTV", "V70.PTV")
# data = data[, keep]

# LUNG
data.survival <- data[,c("survivalstat")]
keep = c('agediag', 'diff_in_days','cum_dosis_specificatie', 
         'DOSESTD.PTV', "V60.PTV", "V65.PTV", "V70.PTV", 'DOSESTD.Longen', 
         "V15.Longen", "V20.Longen", "V25.Longen", 
         'V25.Hart_en_AortaAsc', 'V30.Hart_en_AortaAsc', 'V35.Hart_en_AortaAsc', 
         'V20.Oes', 'V25.Oes', 'V30.Oes')
data = data[, keep]


zero_variance_columns = preprocess_removeZeroVarianceColumns(data)
varied_data <- zero_variance_columns


#vis_dat(data)
#vis_miss(data)
#gg_miss_upset(data)
#n_var_miss(data)
#gg_miss_upset(data, nsets = n_var_miss(data))
#gg_miss_upset(data, nsets = 10, nintersects = NA)

#vis_dat(varied_data)
imputed_varied_data <- preprocess_imputeDataset(varied_data)
#vis_dat(imputed_varied_data)


comb.pca <- prcomp(imputed_varied_data, center = TRUE, scale. = TRUE)

#ggbiplot(comb.pca)



biplot <- ggbiplot(comb.pca, ellipse=TRUE,  groups=data.survival)
print(biplot)

biplot_2 <- ggbiplot(comb.pca, ellipse=TRUE, choices=c(3,4), groups=data.survival)
biplot_2


#ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
#  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
#  ggtitle("PCA of mtcars dataset")+
#  theme_minimal()+
#  theme(legend.position = "right")
