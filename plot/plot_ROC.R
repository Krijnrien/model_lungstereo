library(pROC)
#library(plyr)

# model with dosimetric
# data <- read.csv("data/og_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # upsample
# data2 <- read.csv("data/upsample_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) #og
# data3 <- read.csv("data/downsample_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # undersample

# model with dosimetric, train
# data <- read.csv("data/up_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # upsample
# data2 <- read.csv("data/og_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) #og
# data3 <- read.csv("data/down_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # undersample

data <- read.csv("data/glmnet_up_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # upsample
data2 <- read.csv("data/glmnet_og_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) #og
data3 <- read.csv("data/glmnet_down_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # undersample


# # sim model
# data <- read.csv("data/slim_up_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # upsample
# data2 <- read.csv("data/slim_og_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) #og
# data3 <- read.csv("data/slim_down_test_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # undersample

# # sim model
# data <- read.csv("data/slim_up_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # upsample
# data2 <- read.csv("data/slim_og_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) #og
# data3 <- read.csv("data/slim_down_train_roc.csv", header=TRUE, dec=",", stringsAsFactors=FALSE) # undersample




# convert columns to numerical
data[] <- lapply(data[], function(x) as.numeric(as.character(x)))
data2[] <- lapply(data2[], function(x) as.numeric(as.character(x)))
data3[] <- lapply(data3[], function(x) as.numeric(as.character(x)))


actual_results <- data$outcome
actual_results2 <- data2$outcome
actual_results3 <- data3$outcome


predicted_by_algorithm <- data$prediction
predicted_by_algorithm2 <- data2$prediction
predicted_by_algorithm3 <- data3$prediction

roc_curve <- roc(response = actual_results, predictor = predicted_by_algorithm)
roc_curve2 <- roc(response = actual_results2, predictor = predicted_by_algorithm2)
roc_curve3 <- roc(response = actual_results3, predictor = predicted_by_algorithm3)



# Print the Area Under the ROC curve.
rounded_auc <- round(roc_curve$auc, digits = 2)
rounded_auc2 <- round(roc_curve2$auc, digits = 2)
rounded_auc3 <- round(roc_curve3$auc, digits = 2)

ci <- ci.auc(roc_curve)
ci2 <- ci.auc(roc_curve2)
ci3 <-ci.auc(roc_curve3)

rounded_ci <- round(ci, digits = 2)
rounded_ci2 <- round(ci2, digits = 2)
rounded_ci3 <- round(ci3, digits = 2)


legend_string <- sprintf("upsampled AUC: %s CI: %s (%s - %s)", rounded_auc, rounded_ci[2], rounded_ci[1], rounded_ci[3])
legend_string2 <- sprintf("original AUC: %s CI: %s (%s - %s)", rounded_auc2, rounded_ci2[2], rounded_ci2[1], rounded_ci2[3])
legend_string3 <- sprintf("downsampled AUC: %s CI: %s (%s - %s)", rounded_auc3, rounded_ci3[2], rounded_ci3[1], rounded_ci3[3])

# Add optimal cut-off to ROC curve
e <- cbind(roc_curve$thresholds, roc_curve$sensitivities + roc_curve$specificities)
e2 <- cbind(roc_curve2$thresholds, roc_curve2$sensitivities + roc_curve2$specificities)
e3 <- cbind(roc_curve3$thresholds, roc_curve3$sensitivities + roc_curve3$specificities)


best_cutoff <- subset(e, e[,2] == max(e[,2]))[,1]
best_cutoff2 <- subset(e2, e2[,2] == max(e2[,2]))[,1]
best_cutoff3 <- subset(e3, e3[,2] == max(e3[,2]))[,1]



png('output/roc.png', width = 740, height = 520, units = "px")


plot(1- roc_curve$specificities, roc_curve$sensitivities, type = "l", ylab = "Sensitivity", xlab = "Specificity", col = "black", lwd = 2, main = "ROC Curve");par(new=TRUE)
plot(1- roc_curve2$specificities, roc_curve2$sensitivities, type = "l", col = "#828282", lwd = 2,ylab = "", xlab = "");par(new=TRUE)
plot(1- roc_curve3$specificities, roc_curve3$sensitivities, type = "l", col = "grey", lwd = 2,ylab = "", xlab = "")

abline(v=best_cutoff,col = "black")
abline(v=best_cutoff2, col = "#828282")
abline(v=best_cutoff3, col = "grey")

legend(x = "bottomright", legend=c(legend_string,legend_string2,legend_string3), col=c("black", "#828282","grey"),lwd=1, lty=1)
abline(a = 0, b = 1)

dev.off()

