#install.packages("devtools")
#library(devtools)
#devtools::install_github("vqv/ggbiplot")
#devtools::install_github("kassambara/factoextra") # Required to calculate and plot PC1 variable importance. See base_package_PCA for solution without this package (without variable importance)

library("FactoMineR")
library(ggbiplot)
library("factoextra")
library(ggpubr) #only used for multiplot
library(plyr)


cat("\014") # clear the console
rm(list = ls()) # clear workspace/environment (all variables and functions)
graphics.off() # clear current plots

source("column-selection_stop-treatment.r")
# preprocess functions
source('impute_data.r')

zero_variance_columns = remove_zero_variance_columns(data)
varied_data <- zero_variance_columns
data <- impute_data(varied_data)


data$survivalstat <- factor(data$survivalstat, levels = c(0, 1), labels = c("no-event", "event"))
data.survival <- data[,c("survivalstat")]

# All Longen variables (except mindose (no variance))
all_keep = c('VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen', 'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen')

pca_keep = c('DOSEMEAN.Longen', 'DOSESTD.Longen', 'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen', 'D2PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen')

# After PCA
#pca_keep = c('DOSEMEAN.Longen',  'DOSEMAX.Longen', 'D2CC.Longen',	'D2PRCT_INGY.Longen',	'D98PRCT_INGY.Longen',	'V95PRCT40_05_INPRCT.Longen',	'V95PRCT43_6_INPRCT.Longen',	'V95PRCT53_4_INPRCT.Longen', "V25.Longen", "V30.Longen", "V35.Longen", "V40.Longen", "V45.Longen", "V50.Longen", "V55.Longen")

all_data = data[, all_keep]
all_data <- plyr::rename(all_data, c("VOLUME.Longen"="Longen volume",
                                     "DOSEMEAN.Longen"="mean dose", 
                                     "DOSEMAX.Longen"="max dose", 
                                     "DOSESTD.Longen"="std dose",
                                     "D2PRCT_INGY.Longen"="D2PRCT_INGY",
                                     "D98PRCT_INGY.Longen"="D98PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Longen"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Longen"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Longen"="V95PRCT53_4_INPRCT",
                                     "V5.Longen"="v5",
                                     "V10.Longen"="v10",
                                     "V15.Longen"="v15",
                                     "V20.Longen"="v20",
                                     "V25.Longen"="v25",
                                     "V30.Longen"="v30",
                                     "V35.Longen"="v35",
                                     "V40.Longen"="v40",
                                     "V45.Longen"="v45",
                                     "V50.Longen"="v50",
                                     "V55.Longen"="v55",
                                     "V60.Longen"="v60",
                                     "V65.Longen"="v65"
))
data.pca <- PCA(all_data, graph = FALSE)


pca_data = data[, pca_keep]
pca_data <- plyr::rename(pca_data, c("DOSEMEAN.Longen"="mean dose", 
                                     "DOSESTD.Longen"="std dose",
                                     "D2PRCT_INGY.Longen"="D2PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Longen"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Longen"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Longen"="V95PRCT53_4_INPRCT",
                                     "V15.Longen"="v15",
                                     "V20.Longen"="v20",
                                     "V25.Longen"="v25",
                                     "V30.Longen"="v30",
                                     "V35.Longen"="v35",
                                     "V40.Longen"="v40",
                                     "V45.Longen"="v45",
                                     "V50.Longen"="v50",
                                     "V55.Longen"="v55"
))
pca_data.pca <- PCA(pca_data, graph = FALSE)


pca_var <- fviz_pca_biplot(data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)
dimensions_scree <- fviz_screeplot(data.pca, ncp=10)
var_contribution <- fviz_contrib(data.pca, choice = "var", axes = 1, labelsize=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
selection_pca_var <- fviz_pca_biplot(pca_data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)

# NOTE: Rstudio plot view might not show all plots, click on Zoom!
multiplot <- ggarrange(pca_var, dimensions_scree, var_contribution, selection_pca_var, ncol = 2, nrow = 2)
p <- annotate_figure(multiplot,
                     top = text_grob("Longen - Principal component analysis",face = "bold", size = 14),
                     bottom = text_grob("", hjust = 1, x = 1, face = "italic", size = 10)
)
png('output/lungestereo_lung_PCA.png', width=1920, height=1080)
print(p)
dev.off()

