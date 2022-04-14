library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)
set.seed(1234)

train_data=read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test_data=read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#cleaning the data:
train_data <- train_data[,colMeans(is.na(train_data)) < .9]
train_data <- train_data[,-c(1:7)]

nvz <- nearZeroVar(train_data)
train_data<- train_data[,-nvz]
dim(train_data)

inTrain <- createDataPartition(y=train_data$classe, p=0.7, list=F)
train <- train_data[inTrain,]
valid <- train_data[-inTrain,]

control <- trainControl(method="cv", number=3, verboseIter=F)


mod_trees <- train(classe~., data=train, method="rpart", trControl = control, tuneLength = 5)
fancyRpartPlot(mod_trees$finalModel)

pred_trees <- predict(mod_trees, valid)
cmtrees <- confusionMatrix(pred_trees, factor(valid$classe))
cmtrees


mod_rf <- train(classe~., data=train, method="rf", trControl = control, tuneLength = 5)

pred_rf <- predict(mod_rf, valid)
cmrf <- confusionMatrix(pred_rf, factor(valid$classe))
cmrf

mod_gbm <- train(classe~., data=train, method="gbm", trControl = control, tuneLength = 5, verbose = F)

pred_gbm <- predict(mod_gbm, valid)
cmgbm <- confusionMatrix(pred_gbm, factor(valid$classe))
cmgbm


mod_svm <- train(classe~., data=train, method="svmLinear", trControl = control, tuneLength = 5, verbose = F)

pred_svm <- predict(mod_svm, valid)
cmsvm <- confusionMatrix(pred_svm, factor(valid$classe))
cmsvm

pred <- predict(mod_rf, test_data)
print(pred)

corrPlot <- cor(train[, -length(names(train))])
corrplot(corrPlot, method="color")

plot(mod_trees)

plot(mod_rf)

plot(mod_gbm)
