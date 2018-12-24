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

# All Longen variables (except no variance)
all_keep = c('VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
             'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV',  'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT43_6_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',# dosimetric PTV
             'VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen', 'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
             'VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
)
all_data = data[, all_keep]

# after Cox 0.05 variables
pca_keep = c('DOSEMEAN.Hart_en_AortaAsc', 'DOSESTD.Hart_en_AortaAsc', 'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc', 'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
              # dosimetric PTV (none leftover)
             'DOSEMEAN.Longen', 'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen', 'D2PRCT_INGY.Longen', 'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
             'DOSEMEAN.Oes',  'DOSEMAX.Oes', 'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes', 'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
)
pca_data = data[, pca_keep]
# After PCA
#pca_keep = c('DOSEMEAN.Longen',  'DOSEMAX.Longen', 'D2CC.Longen',	'D2PRCT_INGY.Longen',	'D98PRCT_INGY.Longen',	'V95PRCT40_05_INPRCT.Longen',	'V95PRCT43_6_INPRCT.Longen',	'V95PRCT53_4_INPRCT.Longen', "V25.Longen", "V30.Longen", "V35.Longen", "V40.Longen", "V45.Longen", "V50.Longen", "V55.Longen")

data.pca <- PCA(all_data, graph = FALSE)


pca_data = data[, pca_keep]
pca_data.pca <- PCA(pca_data, graph = FALSE)


pca_var <- fviz_pca_biplot(data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)
dimensions_scree <- fviz_screeplot(data.pca, ncp=10)
var_contribution <- fviz_contrib(data.pca, choice = "var", axes = 1, labelsize=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
selection_pca_var <- fviz_pca_biplot(pca_data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)

# NOTE: Rstudio plot view might not show all plots, click on Zoom!
multiplot <- ggarrange(pca_var, dimensions_scree, var_contribution, selection_pca_var, ncol = 2, nrow = 2)
p <- annotate_figure(multiplot,
                     top = text_grob("All-dosimetric - Principal component analysis",face = "bold", size = 14),
                     bottom = text_grob("", hjust = 1, x = 1, face = "italic", size = 10)
)
print(p)

