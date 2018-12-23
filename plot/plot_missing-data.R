library(naniar)
library(visdat)

source("column-selection_stop-treatment.r")
# preprocess functions
source('impuate_data.r')


vis_dat(data)
gg_miss_upset(data)
n_var_miss(data)

gg_miss_upset(data, nsets = n_var_miss(data))
gg_miss_upset(data, nsets = 10, nintersects = NA)


# Impute data and remove zero variance columns
zero_variance_columns = remove_zero_variance_columns(data)
varied_data <- zero_variance_columns
imputed_varied_data <- impute_data(varied_data)


vis_dat(imputed_varied_data)
imputed_varied_data <- preprocess_imputeDataset(imputed_varied_data)

# Plot all data types including NA
vis_dat(imputed_varied_data)