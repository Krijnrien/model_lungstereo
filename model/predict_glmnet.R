#library(rpart)
#library(glmnet)
library(caret)
#library(visdat)

cat("\014") #clear console
rm(list=ls()) #clear memory
graphics.off # clear plots

set.seed(123)

source("column-selection_stop-treatment.r")
# preprocess functions
source('impute_data.r')

zero_variance_columns = remove_zero_variance_columns(data)
varied_data <- zero_variance_columns
data <- impute_data(varied_data)
rm(zero_variance_columns, varied_data)

data.survival <- data[,c("survivalstat")]

keep = c('survivalstat',
             'DOSEMEAN.Hart_en_AortaAsc', 'DOSESTD.Hart_en_AortaAsc', 'V10.Hart_en_AortaAsc',  'V15.Hart_en_AortaAsc',  'V20.Hart_en_AortaAsc',  'V25.Hart_en_AortaAsc',  'V30.Hart_en_AortaAsc',  'V35.Hart_en_AortaAsc',  'V40.Hart_en_AortaAsc',  'V45.Hart_en_AortaAsc',  'V50.Hart_en_AortaAsc',  'V55.Hart_en_AortaAsc',  'V60.Hart_en_AortaAsc', 'D2CC.Hart_en_AortaAsc',  'D2PRCT_INGY.Hart_en_AortaAsc', 'V95PRCT40_05_INPRCT.Hart_en_AortaAsc',  'V95PRCT43_6_INPRCT.Hart_en_AortaAsc',  'V95PRCT53_4_INPRCT.Hart_en_AortaAsc', #dosimetric heart and aorta
             # dosimetric PTV (none leftover)
             'DOSEMEAN.Longen', 'DOSESTD.Longen',  'V5.Longen',  'V10.Longen',  'V15.Longen',  'V20.Longen',  'V25.Longen',  'V30.Longen',  'V35.Longen',  'V40.Longen',  'V45.Longen',  'V50.Longen',  'V55.Longen',  'V60.Longen', 'D2PRCT_INGY.Longen', 'V95PRCT40_05_INPRCT.Longen',  'V95PRCT43_6_INPRCT.Longen',  'V95PRCT53_4_INPRCT.Longen', #dosimetric Lungs
             'DOSEMEAN.Oes',  'DOSEMAX.Oes', 'DOSESTD.Oes',  'V5.Oes',  'V10.Oes',  'V15.Oes', 'V25.Oes',  'V30.Oes',  'V35.Oes',  'V40.Oes',  'V45.Oes',  'V50.Oes',  'V55.Oes', 'D2CC.Oes',  'D2PRCT_INGY.Oes',  'V95PRCT40_05_INPRCT.Oes',  'V95PRCT43_6_INPRCT.Oes',  'V95PRCT53_4_INPRCT.Oes'#dosimetric oesophagus
)
data = data[, keep]
rm(keep)

data[] <- lapply(data[], function(x) as.numeric(as.character(x)))
# Round all numericals, 0 decimal places
data <- data %>% mutate_if(is.numeric, round, 0)

#vis_dat(data)


smp_size <- floor(0.8 * nrow(data))
## set the seed to make partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]


smp_size <- floor(0.8 * nrow(train))
## set the seed to make partition reproducible
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
pca.train <- train[train_ind, ]
pca.test <- train[-train_ind, ]



prin_comp <- prcomp(pca.train, scale. = T)

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)



#add a training set with principal components
train.data <- data.frame(survivalstat = pca.train$survivalstat, prin_comp$x)

#run a decision tree
#rpart.model <- glmnet(x=as.matrix(train.data), y=train.data$survivalstat)
#apple <- trainControl( method = 'cv', number = 5, classProbs = TRUE)


trainsurv <- factor(train.data$survivalstat)
train.data["survivalstat"] <- NULL
rpart.model <- train(x=train.data, y=trainsurv, method = 'glmnet')

#rpart.model <- rpart(survivalstat ~ .,data = train.data, method = "anova")
rpart.model


#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data)
print(rpart.prediction)




confMat <- table(pca.test$survivalstat, rpart.prediction)
accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy)


