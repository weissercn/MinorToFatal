library(dplyr)
library(caret)
library(nnet)
library(e1071) 
library(class)
library(glmnet)
library(car)
library(MASS)
library(rpart) # for building CART model
library(rpart.plot) # a library for an alternative way of plotting CART trees.
library(caTools)

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


#Create a stepwise logistic regression model
#References:  https://www.rdocumentation.org/packages/MASS/versions/7.3-23/topics/stepAIC
#             https://ashutoshtripathi.com/2019/06/10/what-is-stepaic-in-r/
STEP = stepAIC(clf_logistic)
#Final logistic regression model after the stepAIC process:
STEP = glm(formula = INJ_SEV ~ HOUR_IM + LGTCON_IM + WEATHR_IM + VTRAFWAY + 
      VSURCOND + VTRAFCON + VTCONT_F + PCRASH1_IM + PBSEX + PEDPOS + 
      MOTMAN + PEDCGP + DRIMPAIR + MVISOBSC + NMIMPAIR + MPR_ACT + 
      PBAGE + BDYcats + SPcats + LIMcats, family = "binomial", 
    data = df.small)
summary(STEP)
#the backwards-stepped model has fewer coefficients than the original model
length(coef(clf_logistic))
length(coef(STEP))


#Ordered Logistic Regression (Ordered Logit) Model
#Reference: https://www.rdocumentation.org/packages/MASS/versions/7.3-49/topics/polr
df.order = df.small
df.order$INJ_SEV = factor(df.orig$INJ_SEV, levels=0:4, ordered=TRUE) #convert the binary back to a multilevel scale
#Ordered logistic regression model
orderLogit = polr(INJ_SEV~., data=df.order, Hess=TRUE)
summary(orderLogit)

orderPredict = predict(orderLogit, df.order, type = "probs")
head(orderPredict) #how to interpret these probabilities/compare with logit model?


#CART Model
tree = rpart(INJ_SEV~., data=df.small, minbucket=10) #try different cp values? Deafault is 0.01
prp(tree,tweak=1.2) #recall that 'yes' is always the left branch
rpart.plot(tree,tweak=1.2)








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
