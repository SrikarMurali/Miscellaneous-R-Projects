

library(caret)
data(iris)
dataset <- iris

#training data
validation_index <- createDataPartition(dataset$Species, p=0.8, list=FALSE)

#20% of data for testing
validation <- dataset[-validation_index,]

#remaining 80% for training
dataset <- dataset[validation_index,]

#EDA
dim(dataset)
sapply(dataset, class)
head(dataset)
levels(dataset$Species)

#distribution of the class summary
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

summary(dataset)

#univariate plots
x <- dataset[, 1:4]
y <- dataset[, 5]

#boxplot for each attribute
par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(x[,i], main=names(iris)[i])
  }
#barplot
plot(y)

#scatteplot matrix
library(ellipse)
featurePlot(x=x,y=y, plot='ellipse')

#box and whisker plot for each attribute
featurePlot(x=x,y=y, plot='box')

#density plots for each attribute by class value
scales <- list(x=list(relation='free'), y=list(relation='free'))
featurePlot(x=x,y=y, plot='density', scale=scales)

#run algorithims using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#build models
#a.linear algorithims
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

#b.non linear algorithims
#CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
#kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method='knn', metric=metric, trControl=control)

#c.advanced algorithims
#SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method='svmRadial', metric=metric, trControl=control)
#Random forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method='rf', metric=metric, trControl=control)

#summarize accuracy of each model
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#compare accuracy of models
dotplot(results)
#summarize best model
print(fit.lda)
print(summary(fit.lda))

#estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
