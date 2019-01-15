#fairly sure not using this .R file anymore. yet keeping it. These functions should be embedded in the .r files using them such as glmnet.r and best_classifier.r

library(imputeMissings)

preprocess_dataset <- function(data,data_class,outerFolds,kOuter){
  # This function imputes missing values on all data using train data information, 
  # dummy codes categoricals of all data based on all data, 
  # removes zero variance columns in all data based on train data, 
  # rescales columns to [0,1] for all data & dummied data based on train data.
  
  # initalize all fold-related variables
  trainFoldLabels = trainIndicesOuter = testIndicesOuter = trainDataOuter =trainDummiedDataOuter = trainClassOuter = testDataOuter = testDummiedDataOuter = testClassOuter = trainDataOuter_preImputation = imputedData = dummiedImputedData = dummies = list() 
  
  # remove zero variance columns otherwise dummy coding might fail. Second input is rendundant (improve in net version)
  firstZeroVarianceRemovalOutput = remove_zero_variance_columns(data,data)
  data = firstZeroVarianceRemovalOutput[[1]]
  
  # create variables containing indices for folds, create data frames containing the training and test data 
  for (i_kOuter in 1:kOuter){
    trainFoldLabels[[i_kOuter]] = setdiff(1:kOuter,i_kOuter) # list of vectors containing indices of all folds for the training slice
    
    trainIndicesOuter[[i_kOuter]] = unlist(outerFolds[trainFoldLabels[[i_kOuter]]], use.names = FALSE) # list of vectors containing all patient indices for the training slice
    testIndicesOuter[[i_kOuter]] = outerFolds[[i_kOuter]] # list of vectors containing all patient indices for the test slice
    
    trainDataOuter_preImputation[[i_kOuter]] = data[trainIndicesOuter[[i_kOuter]],] # list of data frames containing the training data slice before it is imputed
    
    imputedData[[i_kOuter]] = impute_data(trainDataOuter_preImputation[[i_kOuter]], data) # run imputation on all data using imputed values from the training slice
    
    # dummy code all factors for the imputed data
    dummies[[i_kOuter]] = dummyVars(' ~.', data = imputedData[[i_kOuter]], fullRank = TRUE) 
    dummiedImputedData[[i_kOuter]] = data.frame(predict(dummies[[i_kOuter]], newdata = imputedData[[i_kOuter]]))
    
    # assign train data
    trainDataOuter[[i_kOuter]] = imputedData[[i_kOuter]][trainIndicesOuter[[i_kOuter]],] # list of data frames containing the training data slice after imputation
    trainDummiedDataOuter[[i_kOuter]] = dummiedImputedData[[i_kOuter]][trainIndicesOuter[[i_kOuter]],] # list of data frames containing the DUMMIED training data slice after imputation
    trainClassOuter[[i_kOuter]] = data_class[trainIndicesOuter[[i_kOuter]]] # list of logical vectors containing the classes for the training data slice
    
    # assign test data
    testDataOuter[[i_kOuter]] = imputedData[[i_kOuter]][testIndicesOuter[[i_kOuter]],] # list of data frames containing the test data slice after imputation
    testDummiedDataOuter[[i_kOuter]] = dummiedImputedData[[i_kOuter]][testIndicesOuter[[i_kOuter]],] # list of data frames containing the test data slice after imputation
    testClassOuter[[i_kOuter]] = data_class[testIndicesOuter[[i_kOuter]]] # list of logical vectors containing the classes for the test data slice
    
    zeroVarianceRemovalOutput = remove_zero_variance_columns(trainDataOuter[[i_kOuter]],testDataOuter[[i_kOuter]])
    trainDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[1]]
    testDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[2]]
    
    zeroVarianceRemovalOutput = remove_zero_variance_columns(trainDummiedDataOuter[[i_kOuter]],testDummiedDataOuter[[i_kOuter]])
    trainDummiedDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[1]]
    testDummiedDataOuter[[i_kOuter]] = zeroVarianceRemovalOutput[[2]]
    
    # rescale data
    preProcessValues = preProcess(trainDataOuter[[i_kOuter]], method = 'range')
    trainDataOuter[[i_kOuter]] = predict(preProcessValues,trainDataOuter[[i_kOuter]])
    testDataOuter[[i_kOuter]] = predict(preProcessValues,testDataOuter[[i_kOuter]])
    
    preProcessDummiedValues = preProcess(trainDummiedDataOuter[[i_kOuter]], method = 'range')
    trainDummiedDataOuter[[i_kOuter]] = predict(preProcessDummiedValues,trainDummiedDataOuter[[i_kOuter]])
    testDummiedDataOuter[[i_kOuter]] = predict(preProcessDummiedValues,testDummiedDataOuter[[i_kOuter]])
  }  
  return(list(trainDataOuter,testDataOuter,trainDummiedDataOuter,testDummiedDataOuter,trainClassOuter,testClassOuter))
}
impute_data <- function(trainData,fullData){
  # This function imputes missing values on all data using train data information,
  # uses medians for continuous variables and modes for categorical variables.
  #only impute if there is any missing value in the full dataset
  if (any(is.na(fullData))){
    imputationValues <- compute(trainData, method = 'median/mode') # compute medians and modes from trainData
    imputedFullData <- impute(fullData, object = imputationValues, flag = FALSE) # replace missing values in the full dataset with medians/modes from trainData; add indicator variables for each column where missing values were imputed
  } else{
    imputedFullData = fullData # if there aren't any missing values, just return the original fullData variable
  }
  
  return(imputedFullData)
}
remove_zero_variance_columns <- function(trainData,testData){
  # This function removes zero variance columns in all data based on train data.
  
  columnsToKeep_levels = sapply(trainData, function(col) length(unique(col))) # count unique (including NA) entries per column in the train data
  columnsToKeep_nas = sapply(trainData, function(col) any(is.na(unique(col)))) # check if at least one NA is present per column in the train data
  columnsToKeep_levels[columnsToKeep_nas] = columnsToKeep_levels[columnsToKeep_nas]-1 # count unique (excluding NA) entries per column in the train data
  columnsToKeep = columnsToKeep_levels>1 # TRUE if column is not unique thus keep column
  newTrainData = trainData[,columnsToKeep] #keep all columns that are not unique in trainData
  newTestData = testData[,columnsToKeep] # keep the same columns also in testData
  return(list(newTrainData,newTestData))
}


