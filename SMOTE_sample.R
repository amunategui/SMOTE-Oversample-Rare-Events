require(DMwR)
require(caret)
require(pROC)

# downlood data set
hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]

# clean column names
names <- gsub(pattern =":|[.]",x = names, replacement="")
colnames(hyper) <- names
colnames(hyper) <-c("target", "age", "sex", "on_thyroxine", "query_on_thyroxine",
  "on_antithyroid_medication", "thyroid_surgery", "query_hypothyroid",
  "query_hyperthyroid", "pregnant", "sick", "tumor", "lithium",
  "goitre", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured",
  "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured",
  "TBG")

# clean up observation values
hyper$target <- ifelse(hyper$target=='negative',0,1)
ind <- sapply(hyper, is.factor)
hyper[ind] <- lapply(hyper[ind], as.character)
hyper[ hyper == "?" ] = NA
hyper[ hyper == "f" ] = 0
hyper[ hyper == "t" ] = 1
hyper[ hyper == "n" ] = 0
hyper[ hyper == "y" ] = 1
hyper[ hyper == "M" ] = 0
hyper[ hyper == "F" ] = 1
hyper[ind] <- lapply(hyper[ind], as.numeric)
repalceNAsWithMean <- function(x) {replace(x, is.na(x), mean(x[!is.na(x)]))}
hyper <- repalceNAsWithMean(hyper)

# confirm rare event
print(table(hyper$target))
print(prop.table(table(hyper$target)))

# split data set in two for training and testing portions
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(hyper$target, p = .50, list = FALSE, times = 1)
trainSplit <- hyper[ splitIndex,]
testSplit <- hyper[-splitIndex,]

# model using treebag
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(target ~ ., data = trainSplit, method = "treebag", trControl = ctrl)

# predict
predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

# score prediction using AUC
library(pROC)
auc <- roc(testSplit$target, pred)
print(auc)

# SMOTE the data
library(DMwR)
trainSplit$target <- as.factor(trainSplit$target)
trainSplit <- SMOTE(target ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$target <- as.numeric(trainSplit$target)
print(prop.table(table(trainSplit$target)))

# re-train and predict
tbmodel <- train(target ~ ., data = trainSplit, method = "treebag", trControl = ctrl)
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

# score SMOTE prediction
auc <- roc(testSplit$target, pred)
print(auc)