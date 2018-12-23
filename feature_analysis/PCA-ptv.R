#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")

library(ggbiplot)


cat("\014") # clear the console
rm(list = ls()) # clear workspace/environment (all variables and functions)
graphics.off() # clear current plots

source("column-selection_stop-treatment.r")
# preprocess functions
source('impuate_data.r')

zero_variance_columns = remove_zero_variance_columns(data)
data <- zero_variance_columns
data <- impute_data(data)


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








data.pca <- prcomp(imputed_varied_data, center = TRUE, scale. = TRUE)


biplot <- ggbiplot(data.pca, ellipse=TRUE,  groups=data.survival)
print(biplot)

biplot_2 <- ggbiplot(data.pca, ellipse=TRUE, choices=c(3,4), groups=data.survival)
biplot_2


#ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
#  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
#  ggtitle("PCA of mtcars dataset")+
#  theme_minimal()+
#  theme(legend.position = "right")
