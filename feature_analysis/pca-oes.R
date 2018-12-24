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

# All Oes variables (except v70, and v75 (no variance))
all_keep = c('VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes')

# after Cox 0.05 variables
pca_keep = c('DOSEMEAN.Oes', 'DOSESTD.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes', 'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes')

# After PCA
#pca_keep = c('DOSEMEAN.Oes',  'DOSEMAX.Oes', 'D2CC.Oes',	'D2PRCT_INGY.Oes',	'D98PRCT_INGY.Oes',	'V95PRCT40_05_INPRCT.Oes',	'V95PRCT43_6_INPRCT.Oes',	'V95PRCT53_4_INPRCT.Oes', "V25.Oes", "V30.Oes", "V35.Oes", "V40.Oes", "V45.Oes", "V50.Oes", "V55.Oes")

all_data = data[, all_keep]
all_data <- plyr::rename(all_data, c("VOLUME.Oes"="oes volume",
                                     "DOSEMEAN.Oes"="mean dose", 
                                     "DOSEMAX.Oes"="max dose", 
                                     "DOSEMIN.Oes"="min dose",
                                     "DOSESTD.Oes"="std dose",
                                     "D2PRCT_INGY.Oes"="D2PRCT_INGY",
                                     "D98PRCT_INGY.Oes"="D98PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Oes"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Oes"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Oes"="V95PRCT53_4_INPRCT",
                                     "V5.Oes"="v5",
                                     "V10.Oes"="v10",
                                     "V15.Oes"="v15",
                                     "V20.Oes"="v20",
                                     "V25.Oes"="v25",
                                     "V30.Oes"="v30",
                                     "V35.Oes"="v35",
                                     "V40.Oes"="v40",
                                     "V45.Oes"="v45",
                                     "V50.Oes"="v50",
                                     "V55.Oes"="v55",
                                     "V60.Oes"="v60",
                                     "V65.Oes"="v65"
))
data.pca <- PCA(all_data, graph = FALSE)


pca_data = data[, pca_keep]
all_data <- plyr::rename(data, c("DOSEMEAN.Oes"="mean dose", 
                                     "DOSEMAX.Oes"="max dose", 
                                     "DOSESTD.Oes"="std dose",
                                     "D2PRCT_INGY.Oes"="D2PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Oes"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Oes"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Oes"="V95PRCT53_4_INPRCT",
                                     "V25.Oes"="v25",
                                     "V30.Oes"="v30",
                                     "V35.Oes"="v35",
                                     "V40.Oes"="v40",
                                     "V45.Oes"="v45",
                                     "V50.Oes"="v50",
                                     "V55.Oes"="v55"
))
pca_data.pca <- PCA(pca_data, graph = FALSE)


pca_var <- fviz_pca_biplot(data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)
dimensions_scree <- fviz_screeplot(data.pca, ncp=10)
var_contribution <- fviz_contrib(data.pca, choice = "var", axes = 1, labelsize=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
selection_pca_var <- fviz_pca_biplot(pca_data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)

# NOTE: Rstudio plot view might not show all plots, click on Zoom!
multiplot <- ggarrange(pca_var, dimensions_scree, var_contribution, selection_pca_var, ncol = 2, nrow = 2)
p <- annotate_figure(multiplot,
                     top = text_grob("Oes - Principal component analysis",face = "bold", size = 14),
                     bottom = text_grob("", hjust = 1, x = 1, face = "italic", size = 10)
)
print(p)

