library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
#install.packages('e1071')
library(e1071) 

df = read.csv("Data/CRSS2017CSV/PERSON.csv", stringsAsFactors = F)

## Binary classification
df$INJ_SEV.cat = df$INJ_SEV >2
df.train$INJ_SEV.cat <- as.factor(df.train$INJ_SEV.cat)
df = df[ , !(names(df) %in% c("INJ_SEV"))]

## HW3 for cross validation


 
# Split data frame into training and test data
set.seed(321)
df.train.indexes <- createDataPartition(df$INJ_SEV, p=0.7, list=FALSE)
df.train <- df[df.train.indexes,]
df.test <- df[-df.train.indexes,]


### Classification techniques

# Logistic

clf_logistic <- glm(INJ_SEV.cat~., data=df.train, family="binomial") #does not converge

# CART class
clf_tree_class = rpart(INJ_SEV.cat~.,
      data = df.train, method="class",
      parms=list(loss=cbind(c(0, 20), c(1, 0))),
      minbucket=5, cp=0.02)

# Random forest #until now used for regression

# https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
# glm, ada, 
# https://topepo.github.io/caret/available-models.html

df.train.noINJ_SEV = subset(df.train, select=-c(INJ_SEV.cat))



rf.cv = train(y = df.train$INJ_SEV.cat,
      x = subset(df.train, select=-c(INJ_SEV.cat)),
      method="rf", nodesize=25, ntree=10,
      trControl=trainControl(method="cv", number=10),  # 10-fold 
      tuneGrid=data.frame(mtry=seq(1,50,1))) 


nnetFit <- train(x=subset(df.train, select=-c(INJ_SEV.cat)), y=df.train$INJ_SEV.cat,
                 method = "nnet",
                 #preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)
knnFit1 <- train(x=subset(df.train, select=-c(INJ_SEV.cat)), y=df.train$INJ_SEV.cat,
                  "knn",
                  tuneLength = 10,
                  trControl = trainControl(method = "cv"))

knnFit1 <- train(x=subset(df.train, select=-c(INJ_SEV.cat)), y=df.train$INJ_SEV.cat,
                 "knn",
                 trControl = trainControl(method = "cv", number=10))

knnFit2 <- train(TrainData, TrainClasses,
                 "knn", tuneLength = 10, 
                 trControl = trainControl(method = "boot"))

clf_rf = rf.cv$finalModel

# SVM
clf_svm = svm(formula = INJ_SEV.cat ~ ., 
                 data = df.train, 
                 type = 'C-classification', 
                 kernel = 'linear')  #radial basis

#Prediction

pred_logistic <- predict(clf_logistic, newdata=df.test, type="response") 
threshold <- 0.2
table(test$INJ_SEV.cat, pred_logistic >= threshold)
loss <- sum(pred_logistic <= threshold & test$INJ_SEV.cat == 0) -
  4 * sum(pred_logistic <= threshold & test$INJ_SEV.cat == 1)

# should adapt this
if(FALSE) {
pred <- prediction(predict(mod, newdata=df.test,
                           type="response"), test$INJ_SEV.cat)
performance(pred, "auc")@y.values[[1]]
rocr.pred.df <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]], tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])
ggplot(rocr.pred.df,aes(x=fpr)) +geom_line(aes(y=tpr),lwd=1)
}


# Improvements :
# Figure out what dependent variable is
# Should we use classification (two or multiclass?) or regression?
# Get half of this  code to run
# Use crossvalidation (lecture 7)


