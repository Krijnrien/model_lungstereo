library(imputeMissings)

impute_data <- function(data){
  # This function imputes missing values on all data using train data information,
  # uses medians for continuous variables and modes for categorical variables.
  #only impute if there is any missing value in the full dataset
  if (any(is.na(data))){
    imputationValues <- compute(data, method = 'median/mode') # compute medians and modes from trainData
    imputeddata <- impute(data, object = imputationValues, flag = FALSE) # replace missing values in the full dataset with medians/modes from trainData; add indicator variables for each column where missing values were imputed
  } else{
    imputeddata = data # if there aren't any missing values, just return the original data variable
  }
  return(imputeddata)
}


remove_zero_variance_columns <- function(data){
  # This function removes zero variance columns in all data based on train data.

  columnsToKeep_levels = sapply(data, function(col) length(unique(col))) # count unique (including NA) entries per column in the train data
  columnsToKeep_nas = sapply(data, function(col) any(is.na(unique(col)))) # check if at least one NA is present per column in the train data
  columnsToKeep_levels[columnsToKeep_nas] = columnsToKeep_levels[columnsToKeep_nas]-1 # count unique (excluding NA) entries per column in the train data
  columnsToKeep = columnsToKeep_levels>1 # TRUE if column is not unique thus keep column
  newdata = data[,columnsToKeep] #keep all columns that are not unique in trainData
  return(newdata)
}