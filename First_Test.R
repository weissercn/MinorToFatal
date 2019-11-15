library(dplyr)
library(caret)

# Load Data
source('data-helpers.R')
df <- data.get_df();


# Split data frame into training and test data
set.seed(321)
train.indexes <- createDataPartition(df$INJ_SEV, p=0.7, list=FALSE)
train <- df[train.indexes,]
test <- df[-train.indexes,]


### Regression techniques

# Linear

lin <- lm(INJ_SEV ~ ., data = train) 
#most important features: REGION.x, URBANICITY.x, AGE, P_SF1, INJSEV_IM, PSUSTRAT.x, WEIGHT.x
# NUM_INJ, MAX_SEV, MAXSEV_IM, NO_INJ_IM
summary(lin)
#cor(train)

# CART

library(rpart)
library(rpart.plot)

tree = rpart(INJ_SEV ~ .,
                   data=train,
                   method="anova",
                   minbucket = 25,
                   cp=0.002)

# Random Forest
# Doesn't work yet for some reason
library(randomForest)
rf = randomForest(INJ_SEV~., data=train,
                      ntree=500,mtry=4,nodesize=5)

sort(importance(rf))

# Prediction
SST <- sum((mean(train$INJ_SEV)-test$INJ_SEV)^2) 

OSR2_lin <- 1- sum((predict(lin, newdata=test)-test$INJ_SEV)^2) / SST #0.939
OSR2_tree <- 1- sum((predict(tree, newdata=test)-test$INJ_SEV)^2) / SST #0.926
OSR2_rf <- 1- sum((predict(rf, newdata=test)-test$INJ_SEV)^2) / SST #doesn't work, yet

### Classification techniques

dall_class <- df
dall_class$INJ_SEV.cat = dall_class$INJ_SEV >3
dall_class = dall_class[ , !(names(dall_class) %in% c("INJ_SEV"))]

train_class <- dall_class[train.indexes,]
test_class <- dall_class[-train.indexes,]

# Logistic

logistic <- glm(INJ_SEV.cat~., data=train_class, family="binomial") #does not converge

# CART class

library(rpart)
library(rpart.plot)
tree_class = rpart(INJ_SEV.cat~.,
      data = train_class, method="class",
      parms=list(loss=cbind(c(0, 20), c(1, 0))),
      minbucket=5, cp=0.02)

#Prediction

pred_logistic <- predict(logistic, newdata=test_class, type="response") 
threshold <- 0.2
table(test_class$INJ_SEV.cat, pred_logistic >= threshold)
loss <- sum(pred_logistic <= threshold & test_class$INJ_SEV.cat == 0) -
  4 * sum(pred_logistic <= threshold & test_class$INJ_SEV.cat == 1)

# should adapt this
if(FALSE) {
pred <- prediction(predict(mod, newdata=test,
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


