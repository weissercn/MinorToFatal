library(dplyr)
library(caret)
library(nnet)
library(e1071) 
library(class)
library(glmnet)
library(car)

#Create the logistic regression model
clf_logistic <- glm(INJ_SEV~., data=df.small, family="binomial")
summary(clf_logistic)

# create a data frame from the output of the logit model's coefficients
coEfs = as.data.frame(summary(clf_logistic)$coefficients)
sigs = coEfs$`Pr(>|z|)` <= 0.05 # include only variables significant at 0.05 level or better
sigCoefs = coEfs[sigs,]
sortSignif = sigCoefs[order(sigCoefs$Estimate),] # sort these significant coefficients descending according to the magnitude of their coefficient
head(sortSignif,60) #show the top 60 significant coefficients

write.csv(sortSignif,'sigVars.csv',row.names = TRUE) #write to a csv file for excel processing

#Use to check the distribution of INJ_SEV for any variable
df.orig$BDYcats = df$BDYcats
dplyr::summarise(group_by(df.orig, df.orig$BDYcats),
                 count = n(),
                 inj_sev = mean(INJ_SEV),
                 inj_sev_std = sd(INJ_SEV))

#Use to check the number of counts for each factor in a variable
dplyr::summarise(group_by(df, df$INJ_SEV),
                 count = n())


# Testing/training code
set.seed(321)
train.indexes <- createDataPartition(df.small$INJ_SEV, p=0.7, list=FALSE)
df.train <- df.small[train.indexes,]
df.test <- df.small[-train.indexes,]

#Train the model using only the training data
clf_logisticTrained <- glm(INJ_SEV~., data=df.train, family="binomial")

##Model Evaluation Metrics
pred_logistic <- predict(clf_logisticTrained, newdata=df.test, type="response") 
threshold <- 0.5
confusion.matrix = table(df.test$INJ_SEV, pred_logistic >= threshold)
confusion.matrix

head(pred_logistic)

# Accuracy, TPR, FPR
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix);
TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,]);
FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,]);
c(Accuracy=accuracy, TPR=TPR, FPR=FPR)

#Baseline Accuracy, TPR, FPR
table(df.train$INJ_SEV) # The baseline is to predict the mode (all FALSE)
accuracy = sum(df.test$INJ_SEV == FALSE)/nrow((df.test))
TPR = 0 #correctly predicts positive class
FPR = 0 #incorrectly predicts the positive class
c(Accuracy=accuracy, TPR=TPR, FPR=FPR) # Currently the model is a slight improvement over the baseline, yay!
# Testing GitHub- Zade
