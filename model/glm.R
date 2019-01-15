# credit to timo & frank
#TODO update with further informatiom and link to source

library(dplyr)
library(plyr)
library(caret)
library(ggplot2)
library(ResourceSelection)
library(imputeMissings)
library(rlist)
library(pROC)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots


cat(sprintf('[%s]\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S')))
startTime_simulation = Sys.time()
timeLabel = format(Sys.time(), '%Y%m%d_%H%M%S')

options(warn = 1) # show each warning when it happensno
options(nwarnings = 10000) # allow many warnings to be saved
options(show.error.locations = TRUE) # show code line numbers of errors

seed <- 1234214	# seed for reproducibility (used before creating outer folds and before each model tuning/training)
outputTable = data.frame(rep = numeric(), dataset = character(), outerFold = numeric(), classifier = character(), innerCvAuc = numeric(), auc = numeric(), calibrationIntercept = numeric(), calibrationSlope = numeric(), brierScore = numeric(), HosLemPvalue = numeric(), myAccuracy = numeric(), myKappa = numeric()) # initialize outputTable


minRep <- 1		# defining the repetition number to start with
maxRep <- 10		# defining the repetition number to end with
kOuter <- 5		# defining the number of folds used in the outer CV
kInner = 5		# defining the number of folds used in the parameter tuning CV


classifierSelection = 'glm'

pathToOutputSubFolder = file.path('Output', timeLabel) # output location for models
dir.create(pathToOutputSubFolder)



# load data
#source("column-selection_stop-treatment.r")
source("factor-grouping.r")


data$survivalstat <- ifelse(data$diff_in_days > 730, 0, data$survivalstat)
#data$survivalstat <- ifelse(data$diff_in_days > 365, 0, data$survivalstat)

data$survivalstat <- factor(data$survivalstat, levels = c(0, 1))
#data <- upSample(x = data,y = data$survivalstat)
data <- downSample(x = data,y = data$survivalstat)

# all
# keep = c('sex', 'WHO', 'PA', 'origin', 'Schedule', 'FEV1', 'size', 'agediag', #Castor (without toxicity)
#          'aantal_tumoren',	'aantal_doelgebieden', 'volgnummer_tumor',	'volgnummer_doelgebied',	'volgnummer_radiotherapie', 'code_localisatie_doelgebied', 'opzet_radiotherapie',	'indicatie_bestraling', 'location_stat', 'cT_stat', 'cN_stat', 'cM_stat', 'survivalstat','cum_dosis_specificatie', #RTHWEB
#          'VOLUME.Hart_en_AortaAsc',  'DOSEMEAN.Hart_en_AortaAsc',  'DOSEMAX.Hart_en_AortaAsc',  'DOSEMIN.Hart_en_AortaAsc',  'DOSESTD.Hart_en_AortaAsc',  'V5.Hart_en_AortaAsc',  'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc',  'V65.Hart_en_AortaAsc',  'V70.Hart_en_AortaAsc',  'V75.Hart_en_AortaAsc',  'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc',  'D98PRCT_INGY.Hart_en_AortaAsc',  'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
#          'VOLUME.PTV',  'DOSEMEAN.PTV',  'DOSEMAX.PTV',  'DOSEMIN.PTV',  'DOSESTD.PTV',  'V5.PTV',  'V10.PTV',  'V15.PTV',  'V20.PTV',  'V25.PTV',  'V30.PTV',  'V35.PTV',  'V40.PTV',  'V45.PTV',  'V50.PTV',  'V55.PTV',  'V60.PTV',  'V65.PTV',  'V70.PTV',  'V75.PTV',  'D2CC.PTV',  'D2PRCT_INGY.PTV',  'D98PRCT_INGY.PTV',  'V95PRCT40_05_INPRCT.PTV',  'V95PRCT43_6_INPRCT.PTV',  'V95PRCT53_4_INPRCT.PTV',# dosimetric PTV
#          'VOLUME.Longen',  'DOSEMEAN.Longen',  'DOSEMAX.Longen',  'DOSEMIN.Longen',  'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen',  'V65.Longen',  'V70.Longen',  'V75.Longen',  'D2CC.Longen',  'D2PRCT_INGY.Longen',  'D98PRCT_INGY.Longen',  'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
#          'VOLUME.Oes',  'DOSEMEAN.Oes',  'DOSEMAX.Oes',  'DOSEMIN.Oes',  'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes',  'V20.Oes',  'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes',  'V60.Oes',  'V65.Oes',  'V70.Oes',  'V75.Oes',  'D2CC.Oes',  'D2PRCT_INGY.Oes',  'D98PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
# )
#keep = c('survivalstat', 'WHO', 'sex', 'V65.Oes', 'DOSEMEAN.PTV', 'DOSEMAX.PTV', 'DOSESTD.PTV', 'D2CC.PTV', 'D2PRCT_INGY.PTV', 'V10.PTV', 'V20.PTV', 'V65.PTV', 'DOSEMAX.Longen', 'DOSEMIN.Oes', 'V25.Oes', 'V30.Oes', 'V35.Oes')

#keep = c('survivalstat', 'WHO', 'sex', 'V65.Oes','DOSEMEAN.PTV', 'DOSEMAX.PTV', 'DOSESTD.PTV', 'D2CC.PTV', 'D2PRCT_INGY.PTV', 'V10.PTV', 'V20.PTV', 'V65.PTV', 'DOSEMAX.Longen', 'DOSEMIN.Oes', 'V25.Oes', 'V30.Oes', 'V35.Oes')

#keep = c('survivalstat', 'WHO', 'sex', 'DOSESTD.PTV')


#keep = c('survivalstat', 'WHO', 'sex')


#keep = c('sex', 'WHO', 'age_group','survivalstat')
data = data[, keep]

sink(file.path(pathToOutputSubFolder, "used_columns_in_model.txt"))
print(keep)
sink()

rm(keep)




data_class = data$survivalstat # place outcome in separate variable
data$survivalstat = NULL #remove outcome from data
data_class = revalue(data_class, c('0' = 'nonEvent','1' = 'event')) # relabel outcome as event and nonEvent: 0:dead before 2yr --> nonEvent, 1:alive after 2yr --> event
data <- list(data,data_class) 



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



runClassifier <- function(trainData,trainClass,testData,testClass,kInner,defaultTuning,seed, i_kOuter,i_rep){
  # This function fits one classifier on the current training set using a kInner-fold inner CV (using caret),
  # hyperparameters are tuned in the inner CV,
  # evaluates the model on the test set,
  # computes metrics on test set.
  
  # set fitControl for all classifiers.
  fitControl = trainControl( method = 'cv', number = kInner, classProbs = TRUE, allowParallel = FALSE, summaryFunction = twoClassSummary, verboseIter = FALSE)
  tuneGrid <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 100))
  
  
  modelFit <- train(x = trainData, y = trainClass, method = 'glm', maxit = 1000000, trControl = fitControl, metric = 'ROC')
  # retrieve the highest AUC achieved by a hyperparameter-combination in the inner-CV. This should be by the model selected by the inner-CV.
  innerCvAuc = max(modelFit$results$ROC)
  
  # get AUCs via caret::twoClassSummary
  testDf = as.data.frame(predict(modelFit,newdata = testData, type = 'prob')) # create data frame with probabilities for 'event' and 'nonEvent'
  # NOTE: the columns 'event' and 'nonEvent' in testDf are probabilities for those classes! Names cannot be changed because these names are required by twoClassSummary()
  testDf$obs = testClass # assign the true test classes as observations
  testDf$pred = predict(modelFit,newdata = testData) # assign predictions (event or nonEvent) as pred (they are either event or nonEvent). Uses 0.5-cutoff
  
  # note: break if there is still any NA found in the pred column
  if (any(is.na(testDf$pred))) {
    stop('NA in pred found')
  }
  
  predResult <- testDf
  predResult["nonEvent"] <- NULL
  predResult["pred"] <- NULL
  predResult["obs"] <- ifelse(testDf$obs == 'event',1,0)

  obs_pred_label = paste(timeLabel,'_TEST_observed-pred_i-rep','-',toString(i_rep), 'k_outer','-',toString(i_kOuter), sep = '')
  write.csv(predResult, file = file.path(pathToOutputSubFolder, paste(obs_pred_label,'.csv',sep = '')))

  # get AUCs via caret::twoClassSummary
  trainDf = as.data.frame(predict(modelFit,newdata = trainData, type = 'prob')) # create data frame with probabilities for 'event' and 'nonEvent'
  # NOTE: the columns 'event' and 'nonEvent' in trainDf are probabilities for those classes! Names cannot be changed because these names are required by twoClassSummary()
  trainDf$obs = trainClass # assign the true train classes as observations
  trainDf$pred = predict(modelFit,newdata = trainData) # assign predictions (event or nonEvent) as pred (they are either event or nonEvent). Uses 0.5-cutoff
  
  
  predResult <- trainDf
  predResult["nonEvent"] <- NULL
  predResult["pred"] <- NULL
  predResult["obs"] <- ifelse(trainDf$obs == 'event',1,0)
  
  obs_pred_label = paste(timeLabel,'train_observed-pred','-',toString(i_rep), 'k_outer','-',toString(i_kOuter), sep = '')
  write.csv(predResult, file = file.path(pathToOutputSubFolder, paste(obs_pred_label,'.csv',sep = '')))
  
  flush.console()

  # compute auc
  testPerformance = twoClassSummary(data = testDf,lev = levels(testDf$obs), model = modelFit$method) # compute statistics for test set
  auc = testPerformance[1]
  cat(sprintf('AUC=%.2f\n', auc))
  
  # calibration
  testDf$numericObs = ifelse(testDf$obs == 'event',1,0) # code event as 1 and nonEvent as 0 for regression
  calibrationRegression = lm(numericObs ~ event, data = testDf)
  calibrationIntercept = calibrationRegression$coefficients[1]
  calibrationSlope = calibrationRegression$coefficients[2] 
  
  # mean squared error or Brier score
  brierScore = mean((testDf$numericObs - testDf$event)^2)
  
  # Hosmer-Lemeshow GOF test
  hlTest = hoslem.test(x = testDf$event,y = testDf$numericObs,g = 10)
  HosLemPvalue = hlTest$p.value
  
  # accuracy and kappa via caret::confusionMatrix
  cmOutput = caret::confusionMatrix(data = testDf$pred, reference = testDf$obs) # specify the use of confusionMatrix() from the caret function to avoid a conflict with package 'ModelMetrics'
  myAccuracy = cmOutput$overall['Accuracy']
  myKappa = cmOutput$overall['Kappa']
  
  # put statistics in a data frame
  statsDf = data.frame(innerCvAuc, auc, calibrationIntercept, calibrationSlope, brierScore, HosLemPvalue, myAccuracy, myKappa)
  
  # return the data frame with statistics and the fitted model (for saving purposes)
  return(list(statsDf,modelFit))
}
runCvForClassifiers <- function(data,data_class,kOuter,kInner,classifierNames,defaultTuning,seed){
  # This function determines folds and creates train/test sets, 
  # preprocesses the sets,
  # loops over each classifier for each train/test set combination.
  
  
  # initalize modelFitList
  modelFitList = list(length = length(classifierNames))
  varImportance = list(length = length(classifierNames))
  for (i_classifierNames in 1:length(classifierNames)){
    # initialize modelFitList which is a list of lists. One list for each classifier, each contains one entry for each fold.
    modelFitList[[i_classifierNames]] = list(length = kOuter)
    varImportance = list(length = kOuter)
  }
  
  # initialize outputTable
  outputTable = data.frame(outerFold = numeric(), classifier = character(), innerCvAuc = numeric(), auc = numeric(), calibrationIntercept = numeric(), calibrationSlope = numeric(), brierScore = numeric(), HosLemPvalue = numeric(), myAccuracy = numeric(), myKappa = numeric())
  
  # create outer folds
  set.seed(seed) # set seed for reproducibility of folds
  outerFolds = createFolds(y = data_class, k = kOuter) # create folds #NOTE: sometimes the folds are not stratified for the outcome if there are too few events or nonEvents
  
  # create variables containing indices for folds, create data frames containing the training and test data 
  preprocess_datasetOutput = preprocess_dataset(data,data_class,outerFolds,kOuter)
  trainDataOuter = preprocess_datasetOutput[[1]]
  testDataOuter = preprocess_datasetOutput[[2]]
  trainDummiedDataOuter = preprocess_datasetOutput[[3]]
  testDummiedDataOuter = preprocess_datasetOutput[[4]]
  trainClassOuter = preprocess_datasetOutput[[5]]
  testClassOuter = preprocess_datasetOutput[[6]]
  
  # loop over all folds (repeat analysis kOuter-many times)
  for (i_kOuter in 1:kOuter){
    # print current iteration nr
    cat(sprintf('\tOuter fold %d/%d\t\t\t\t\t\t\t\t prevalence=%g%%\t (%d/%d)\n', i_kOuter, kOuter, round(100*sum(trainClassOuter[[i_kOuter]] == 'event')/length(trainClassOuter[[i_kOuter]])), sum(trainClassOuter[[i_kOuter]] == 'event'), length(trainClassOuter[[i_kOuter]]))) # DEBUG
    
    # loop over each classifier
    # print current classifier to be run (newline is printed after AUC inside the runCLassifier function)
    cat(sprintf('\t\tClassifier \t %-10s\t', classifierNames[i_classifierNames]))
    
    # for glm check for near-zero variance columns
    trainDummiedNonNearZeroVarianceDataOuter = trainDummiedDataOuter[[i_kOuter]]
    testDummiedNonNearZeroVarianceDataOuter = testDummiedDataOuter[[i_kOuter]]
    toRemove = nearZeroVar(trainDummiedNonNearZeroVarianceDataOuter)
    
    # if there are near-zero variance variables, remove them from train and test data
    if (length(toRemove)>0) {
      trainDummiedNonNearZeroVarianceDataOuter = trainDummiedNonNearZeroVarianceDataOuter[,-toRemove]
      testDummiedNonNearZeroVarianceDataOuter = testDummiedNonNearZeroVarianceDataOuter[,-toRemove]  
    }
    
    runClassifierOutput = runClassifier(trainDummiedNonNearZeroVarianceDataOuter,trainClassOuter[[i_kOuter]],testDummiedNonNearZeroVarianceDataOuter,testClassOuter[[i_kOuter]],kInner,defaultTuning,seed, i_kOuter, i_rep)
    
    newRow = runClassifierOutput[[1]]
    newRow['outerFold'] = i_kOuter # add outer fold number to new result
    outputTable = rbind(outputTable, newRow) # attach new result to outputTable
    modelFitList[[i_classifierNames]][[i_kOuter]]= runClassifierOutput[[2]] # store fitted model for current dataset/rep combination: store the fitted model for fold i_kOuter in the list for classifier i_classifierNames (in the i_kOuter-th position in the i_classifierNames-th list)
    
    imp <- varImp(runClassifierOutput[[2]])$importance
    varimp_kouter = paste(timeLabel,'_variable-importance_k_rep','-',toString(i_rep), 'k_outer','-',toString(i_kOuter), sep = '')
    write.csv(imp, file = file.path(pathToOutputSubFolder, paste(varimp_kouter,'.csv',sep = '')))
  }
  return(list(outputTable,modelFitList, varImportance))
}

cat(sprintf('Running classifiers on dataset...\n'))
# loop over repetitions
for (i_rep in minRep:maxRep) {
  # print current classifier rep number
  timeleft = ((Sys.time() - startTime_simulation)/(i_rep-minRep)) * (maxRep-i_rep+1) # average time per rep * number of reps left
  cat(sprintf('  Repetition %d/%d\t\t\t\t\t\t\t\t\t\t\t\t timeleft~%.1f %s\n', i_rep, maxRep, timeleft, units(timeleft)))
  
  seedPerRep = seed + i_rep
  analysisOutput = runCvForClassifiers(data[[1]],data[[2]],kOuter,kInner,classifierSelection,defaultTuning,seedPerRep) # function that applies all classifiers
  
  newRows = analysisOutput[[1]] # clumsy way of storing function output in separate variables
  newRows['rep'] = i_rep # add repetition number to new result
  outputTable = rbind(outputTable,newRows) # append new result to outputTable
  #currentModelFits =  rbind(currentModelFits, analysisOutput[[2]]) # clumsy way of storing function output in separate variables
  
  # save current modelFits
  outputFilename = paste(timeLabel,'_model_fits',toString(i_rep),'_dataset',"lungstereo", sep = '')
  #save(list = 'currentModelFits', file = file.path(pathToOutputSubFolder, paste(outputFilename,'.RData',sep = '')))
  # save outputTable to csv for each repetition
  outputFilenameOutputTable = paste(timeLabel,'_models_output_table_rep','-',toString(i_rep), sep = '')
}

write.csv(outputTable, file = file.path(pathToOutputSubFolder, paste(outputFilenameOutputTable,'.csv',sep = '')))


# display running time
endTime = Sys.time()
runningTime = endTime - startTime_simulation
print(runningTime)



parametersToSave = c('outputTable', 'analysisOutput', 'classifierSelection', 'kInner', 'kOuter', 'minRep', 'maxRep', 'startTime_simulation','endTime','runningTime')
outputFilename = paste(timeLabel,'_main_simulation_output_seeds', sep = '')
save(list = parametersToSave, file = file.path(pathToOutputSubFolder,paste(outputFilename,'.RData',sep = '')))