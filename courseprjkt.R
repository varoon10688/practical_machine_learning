library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(randomForest)

set.seed(0)
library(rattle)

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

testing_ult <- read.csv("pml-testing.csv",na.strings = c("NA", ""))
data <- read.csv("pml-training.csv",na.strings = c("NA", ""))


inTrain = createDataPartition(y=data$classe, p=0.6, list=FALSE)

inTrain = createFolds(y=data$classe, k=10, list=FALSE)

training = data[inTrain,]
testing = data[-inTrain,]



na_train = sapply(training, function(x) {sum(is.na(x))})
table(na_train)
good_columns = (names(na_train[na_train==0]))

training = training[, names(training) %in% good_columns]
training = training[,-c(1:7)]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

model_rpart = train(classe~., method="rpart", data=training,trControl = fitControl)

model_rf = train(classe~., method="rf", data=training, prox=TRUE, do.trace =100,trControl = fitControl)

confusionMatrix(training$classe,predict(model_rf,training))
varImp(model_rf)
stopCluster(cl)
