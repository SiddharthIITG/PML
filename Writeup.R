library("caTools")
library("randomForest")
library("rpart")
library("rpart.plot")

train = read.csv("pml-training.csv")
test = read.csv("pml-testing.csv")
train = train[, colSums(is.na(train)) < 5]
train = train[, colSums(train != "") > 1000]
test = test[, colSums(is.na(test)) < 5]
train = train[, 8:60]
test = test[,8:60]

split = sample.split(train$classe, SplitRatio = 0.7)
trainData = subset(train, split==TRUE)
testData = subset(train, split==FALSE)

nzv <- nearZeroVar(trainData, saveMetrics=TRUE)
nzv

controlRf <- trainControl(method="cv", 5)
rfModel <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=200)
rfModel
predRf <- predict(rfModel, testData)
confusionMatrix(testData$classe, predRf)

predTest <- predict(rfModel,test[, -length(names(test))])
predTest

treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel) 
