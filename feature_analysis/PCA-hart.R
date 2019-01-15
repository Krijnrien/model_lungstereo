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
all_keep = c('VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc') #dosimetric heart and aorta
#all_keep = c('VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc') #dosimetric heart and aorta


# After PCA
pca_keep = c('DOSEMEAN.Hart_en_AortaAsc', 'DOSESTD.Hart_en_AortaAsc', 'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc', 'D2PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc') #dosimetric heart and aorta

all_data = data[, all_keep]
all_data <- plyr::rename(all_data, c("VOLUME.Hart_en_AortaAsc"="Hart_en_AortaAsc volume",
                                     "DOSEMEAN.Hart_en_AortaAsc"="mean dose", 
                                     "DOSEMAX.Hart_en_AortaAsc"="max dose", 
                                     "DOSESTD.Hart_en_AortaAsc"="std dose",
                                     "D2PRCT_INGY.Hart_en_AortaAsc"="D2PRCT_INGY",
                                     "D98PRCT_INGY.Hart_en_AortaAsc"="D98PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Hart_en_AortaAsc"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Hart_en_AortaAsc"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Hart_en_AortaAsc"="V95PRCT53_4_INPRCT",
                                     "V5.Hart_en_AortaAsc"="v5",
                                     "V10.Hart_en_AortaAsc"="v10",
                                     "V15.Hart_en_AortaAsc"="v15",
                                     "V20.Hart_en_AortaAsc"="v20",
                                     "V25.Hart_en_AortaAsc"="v25",
                                     "V30.Hart_en_AortaAsc"="v30",
                                     "V35.Hart_en_AortaAsc"="v35",
                                     "V40.Hart_en_AortaAsc"="v40",
                                     "V45.Hart_en_AortaAsc"="v45",
                                     "V50.Hart_en_AortaAsc"="v50",
                                     "V55.Hart_en_AortaAsc"="v55",
                                     "V60.Hart_en_AortaAsc"="v60"
))
data.pca <- PCA(all_data, graph = FALSE)


pca_data = data[, pca_keep]
all_data <- plyr::rename(pca_data, c("DOSEMEAN.Hart_en_AortaAsc"="mean dose", 
                                     "DOSESTD.Hart_en_AortaAsc"="std dose",
                                     "D2PRCT_INGY.Hart_en_AortaAsc"="D2PRCT_INGY",
                                     "V95PRCT40_05_INPRCT.Hart_en_AortaAsc"="V95PRCT40_05_INPRCT",
                                     "V95PRCT43_6_INPRCT.Hart_en_AortaAsc"="V95PRCT43_6_INPRCT",
                                     "V95PRCT53_4_INPRCT.Hart_en_AortaAsc"="V95PRCT53_4_INPRCT",
                                     "V15.Hart_en_AortaAsc"="v15",
                                     "V20.Hart_en_AortaAsc"="v20",
                                     "V25.Hart_en_AortaAsc"="v25",
                                     "V30.Hart_en_AortaAsc"="v30",
                                     "V35.Hart_en_AortaAsc"="v35",
                                     "V40.Hart_en_AortaAsc"="v40",
                                     "V45.Hart_en_AortaAsc"="v45",
                                     "V50.Hart_en_AortaAsc"="v50"
))
pca_data.pca <- PCA(pca_data, graph = FALSE)


pca_var <- fviz_pca_biplot(data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)
dimensions_scree <- fviz_screeplot(data.pca, ncp=10)
var_contribution <- fviz_contrib(data.pca, choice = "var", axes = 1, labelsize=1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
selection_pca_var <- fviz_pca_biplot(pca_data.pca, label="var", labelsize=2, col.var="black", habillage=data.survival, addEllipses=TRUE, ellipse.level=0.95)

# NOTE: Rstudio plot view might not show all plots, click on Zoom!
multiplot <- ggarrange(pca_var, dimensions_scree, var_contribution, selection_pca_var, ncol = 2, nrow = 2)

p <- annotate_figure(multiplot,
                     top = text_grob("Hart_en_AortaAsc - Principal component analysis",face = "bold", size = 14),
                     bottom = text_grob("", hjust = 1, x = 1, face = "italic", size = 10)
)

png('output/lungestereo_hart_PCA.png', width=1920, height=1080)
print(p)
dev.off()

