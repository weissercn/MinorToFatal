library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)
#install.packages('e1071')
library(e1071) 
library(randomForest)
library(class)
library(glmnet)

# Load Data
source('data-helpers.R')
df.orig <- data.get_df();

## Binary classification
df <- df.orig
df$INJ_SEV <- as.factor(df.orig$INJ_SEV > 2)
#df = df[ , !(names(df) %in% c("INJ_SEV"))]


# Split data frame into training and test data
set.seed(321)
df.train.indexes <- createDataPartition(df$INJ_SEV, p=0.7, list=FALSE)
df.train <- df[df.train.indexes,]
df.test <- df[-df.train.indexes,]


### Classification techniques

# Logistic
clf_logistic <- glm(INJ_SEV~., data=df.train, family="binomial") #does not converge


# Regularized Regression
set.seed(123) 
lasso.cv <- cv.glmnet(model.matrix(INJ_SEV~., df.train)[,-1], df.train$INJ_SEV, alpha = 1, family = "binomial")
clf_reg <- glmnet(model.matrix(INJ_SEV~., df.train)[,-1], df.train$INJ_SEV, 
                  alpha = 1, family = "binomial",
                  lambda = lasso.cv$lambda.min)

# CART
cart.cv <- train(INJ_SEV~.,
                 data = df.train,
                 method = "rpart",
                 trControl = trainControl(method="cv", number=10),
                 #weights = ifelse(df.train$INJ_SEV == 1, 1, 1), # TODO: should we weight false positives or negatives differently?
                 #metric = "Accuracy",
                 tuneGrid = data.frame(.cp=seq(0.001, 0.06, by=0.002)))

# Retrain model with best CP value
clf_cart = rpart(INJ_SEV~.,
                 data = df.train,
                 method="class",
                 #parms=list(loss=cbind(c(0, 1), c(1, 0))), # TODO: should we weight false positives or negatives differently?
                 minbucket=5, cp=cart.cv$bestTune$cp)

# Random forest #until now used for regression

# https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
# glm, ada, 
# https://topepo.github.io/caret/available-models.html
rf.cv = train(INJ_SEV ~.,
      data=df.train,
      method="rf",
      nodesize=25, ntree=10,  # TODO: why these values?
      trControl=trainControl(method="cv", number=10),  # 10-fold 
      tuneGrid=data.frame(mtry=seq(1,50,1)));
clf_rf = randomForest(INJ_SEV ~.,
                      data=df.train,
                      mtry=rf.cv$bestTune$mtry, 
                      nodesize=25, ntree=10);


# Neural Network
nnet.fit <- train(INJ_SEV ~.,
                   data=df.train,
                   method = "nnet",
                   #preProcess = "range", 
                   tuneLength = 2,
                   trace = FALSE,
                   maxit = 100)
clf_nnet = nnet(INJ_SEV ~.,
                data = df.train,
                size = nnet.fit$bestTune$size,
                decay = nnet.fit$bestTune$decay)

# SVM
clf_svm = svm(formula = INJ_SEV ~ ., 
              data = df.train, 
              type = 'C-classification', 
              kernel = 'linear')  #radial basis

#Linear Regression Prediction
pred_logistic <- predict(clf_logistic, newdata=df.test, type="response") 
threshold <- 0.5
confusion.matrix = table(df.test$INJ_SEV, pred_logistic >= threshold)

# Accuracy, TPR, FPR
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix);
TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,]);
FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,]);
c(Accuracy=accuracy, TPR=TPR, FPR=FPR)

# Regularization Prediction
pred_reg <- predict(clf_reg, newx=model.matrix(INJ_SEV~., df.test)[,-1]) 
threshold <- 0.5
confusion.matrix = table(df.test$INJ_SEV, pred_reg >= threshold)

# Accuracy, TPR, FPR
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix);
TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,]);
FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,]);
c(Accuracy=accuracy, TPR=TPR, FPR=FPR)


#Helper to validate classification models
validate_clf <- function(model) {
  # Make prediction and build confusion matrix
  pred = predict(model, newdata=df.test, type="class");
  confusion.matrix = table(df.test$INJ_SEV, pred);
  
  # Accuracy, TPR, FPR
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix);
  TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,]);
  FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,]);
  
  c(Accuracy=accuracy, TPR=TPR, FPR=FPR)
}

# Output the performance of each model
validate_clf(clf_svm)
validate_clf(clf_nnet)
validate_clf(clf_cart)
validate_clf(clf_rf)

# Other outputs that are useful.
importance(clf_rf)
clf_cart$variable.importance
clf_reg$beta[which(abs(clf_reg$beta) > 0 ),]
