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

# All PTV variables
all_keep = c('VOLUME.PTV', 'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV', 'D2CC.PTV',	'D2PRCT_INGY.PTV',	'D98PRCT_INGY.PTV',	'V95PRCT40_05_INPRCT.PTV',	'V95PRCT43_6_INPRCT.PTV',	'V95PRCT53_4_INPRCT.PTV', "V5.PTV", "V10.PTV", "V15.PTV", "V20.PTV","V25.PTV", "V30.PTV", "V35.PTV", "V40.PTV", "V45.PTV", "V50.PTV", "V55.PTV", "V60.PTV", "V65.PTV", "V70.PTV", "V75.PTV")

# after Cox 0.05 variables
#keep = c('DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSESTD.PTV', 'D2CC.PTV',	'D2PRCT_INGY.PTV', 'V95PRCT40_05_INPRCT.PTV', "V10.PTV", "V15.PTV", "V20.PTV", "V25.PTV", "V30.PTV", "V35.PTV")

# After PCA
pca_keep = c('VOLUME.PTV', 'DOSEMEAN.PTV',  'DOSEMAX.PTV', 'D2CC.PTV',	'D2PRCT_INGY.PTV',	'D98PRCT_INGY.PTV',	'V95PRCT40_05_INPRCT.PTV',	'V95PRCT43_6_INPRCT.PTV',	'V95PRCT53_4_INPRCT.PTV', "V25.PTV", "V30.PTV", "V35.PTV", "V40.PTV", "V45.PTV", "V50.PTV", "V55.PTV")

all_data = data[, all_keep]
all_data <- plyr::rename(all_data, c("VOLUME.PTV"="ptv volume", 
                                     "DOSEMEAN.PTV"="mean dose", 
                                     "DOSEMAX.PTV"="max dose", 
                                     "DOSEMIN.PTV"="min dose",
                                     "DOSESTD.PTV"="std dose",
                                     "D2PRCT_INGY.PTV"="D2PRCT_INGY",
                                     "D98PRCT_INGY.PTV"="D98PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.PTV"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.PTV"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.PTV"="V95PRCT53_4_INPRCT",
                                     "V5.PTV"="v5",
                                     "V10.PTV"="v10",
                                     "V15.PTV"="v15",
                                     "V20.PTV"="v20",
                                     "V25.PTV"="v25",
                                     "V30.PTV"="v30",
                                     "V35.PTV"="v35",
                                     "V40.PTV"="v40",
                                     "V45.PTV"="v45",
                                     "V50.PTV"="v50",
                                     "V55.PTV"="v55",
                                     "V60.PTV"="v60",
                                     "V65.PTV"="v65",
                                     "V70.PTV"="v70",
                                     "V75.PTV"="v75"
))
data.pca <- PCA(all_data, graph = FALSE)


pca_data = data[, pca_keep]
pca_data <- plyr::rename(pca_data, c("DOSEMEAN.PTV"="mean dose", 
                                     "DOSEMAX.PTV"="max dose", 
                                     "D2PRCT_INGY.PTV"="D2PRCT_INGY",
                                     "D98PRCT_INGY.PTV"="D98PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.PTV"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.PTV"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.PTV"="V95PRCT53_4_INPRCT",
                                     "V25.PTV"="v25",
                                     "V30.PTV"="v30",
                                     "V35.PTV"="v35",
                                     "V40.PTV"="v40",
                                     "V45.PTV"="v45",
                                     "V50.PTV"="v50",
                                     "V55.PTV"="v55"
))
pca_data.pca <- PCA(pca_data, graph = FALSE)





pca_var <- fviz_pca_biplot(data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)
dimensions_scree <- fviz_screeplot(data.pca, ncp=10)
var_contribution <- fviz_contrib(data.pca, choice = "var", axes = 1, labelsize=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
selection_pca_var <- fviz_pca_biplot(pca_data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)

# NOTE: Rstudio plot view might not show all plots, click on Zoom!
multiplot <- ggarrange(pca_var, dimensions_scree, var_contribution, selection_pca_var, ncol = 2, nrow = 2)
p <- annotate_figure(multiplot,
                top = text_grob("PTV - Principal component analysis",face = "bold", size = 14),
                bottom = text_grob("", hjust = 1, x = 1, face = "italic", size = 10)
)
print(p)

