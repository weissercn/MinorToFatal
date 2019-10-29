acc = read.csv("Data/CRSS2017CSV/ACCIDENT.csv", stringsAsFactors = F)
pers = read.csv("Data/CRSS2017CSV/PERSON.csv", stringsAsFactors = F)
pb = read.csv("Data/CRSS2017CSV/PBTYPE.csv", stringsAsFactors = F)



pers_proc = pers[which(pers$VEH_NO==0),]
#need to drop some variables with NaNs
pers_proc = pers_proc[ , !(names(pers_proc) %in% c("MAKE","BODY_TYP", "MOD_YEAR", "MAK_MOD", "TOW_VEH", "SPEC_USE", "EMER_USE", "ROLLOVER", "IMPACT1", "FIRE_EXP"))]

#Inner join #keeps only if there is a match
dper <- merge(pers_proc, pb, by=c("CASENUM","PER_NO"))

#Outer join
#dall <- merge(pers_proc, pb, by=c("CASENUM","PER_NO"), all=TRUE)

dall <- merge(dper, acc, by="CASENUM")
#drop more features that cause NaNs
dall = dall[ , !(names(dall) %in% c("VEH_NO.x","SEAT_POS", "REST_USE", "REST_MIS", "AIR_BAG", "EJECTION" ))]
dall = dall[ , !(names(dall) %in% c("DRUGTST2", "DRUGTST3", "DRUGRES2", "DRUGRES3", "P_SF3", "EJECT_IM", "SEAT_IM", "VEH_NO.y", "REGION.y", "PSU.y"))]
dall = dall[ , !(names(dall) %in% c("PJ.y", "PSU_VAR.y", "URBANICITY.y", "STRATUM.y", "PBPTYPE", "PBAGE", "PBSEX" ))]
dall = dall[ , !(names(dall) %in% c("PSUSTRAT.y", "WEIGHT.y", "REGION", "PSU", "PJ", "PSU_VAR", "URBANICITY", "STRATUM", "VE_FORMS.y", "PVH_INVL"))]
dall = dall[ , !(names(dall) %in% c("MONTH.y", "YEAR", "HOUR.y", "MINUTE.y", "MAN_COLL.y", "SCH_BUS.y", "WKDY_IM", "EVENT1_IM", "MANCOL_IM", "PSUSTRAT", "WEIGHT"  ))]
dall = dall[ , !(names(dall) %in% c("HARM_EV.y"))]



train <- dall[dall$CASENUM<=201701000000,]
test <- dall[dall$CASENUM<=201701000000,]


### Regression techniques

# Linear

lin <- lm(HARM_EV.x ~ ., data = train) 
#most important features: PER_TYP, PEDPOS, VE_TOTAL, PEDS, PERNOTMVIT, RELJCT2_IM
summary(lin)
#cor(train)

# CART

library(rpart)
library(rpart.plot)

tree = rpart(HARM_EV.x ~ .,
                   data=train,
                   method="anova",
                   minbucket = 25,
                   cp=0.002)

# Random Forest
# Doesn't work yet for some reason
library(randomForest)
rf = randomForest(HARM_EV.x~., data=train,
                      ntree=500,mtry=4,nodesize=5)

sort(importance(rf))

# Prediction
SST <- sum((mean(train$HARM_EV.x)-test$HARM_EV.x)^2) 

OSR2_lin <- 1- sum((predict(lin, newdata=test)-test$HARM_EV.x)^2) / SST #0.35
OSR2_tree <- 1- sum((predict(tree, newdata=test)-test$HARM_EV.x)^2) / SST #0.41
OSR2_rf <- 1- sum((predict(rf, newdata=test)-test$HARM_EV.x)^2) / SST #doesn't work, yet

### Classification techniques

dall_class = dall
dall_class$HARM_EV.x.cat = dall_class$HARM_EV.x >9
dall_class = dall_class[ , !(names(dall) %in% c("HARM_EV.x"))]

train_class <- dall_class[dall_class$CASENUM<=201701000000,]
test_class <- dall_class[dall_class$CASENUM<=201701000000,]

train$HARM_EV.x.cat = train$HARM_EV.x >9

# Logistic

logistic <- glm(HARM_EV.x.cat~., data=train_class, family="binomial") 

# CART class

library(rpart)
library(rpart.plot)
tree_class = rpart(HARM_EV.x.cat~.,
      data = train, method="class",
      parms=list(loss=cbind(c(0, 20), c(1, 0))),
      minbucket=5, cp=0.02)

#Prediction

pred_logistic <- predict(logistic, newdata=test_class, type="response") 
threshold <- 0.2
table(test_class$HARM_EV.x.cat, pred_logistic >= threshold)
loss <- sum(pred_logistic <= threshold & test_class$HARM_EV.x.cat == 0) -
  4 * sum(pred_logistic <= threshold & test_class$HARM_EV.x.cat == 1)

# should adapt this
if(FALSE) {
pred <- prediction(predict(mod, newdata=test,
                           type="response"), test$Churn)
performance(pred, "auc")@y.values[[1]]
rocr.pred.df <- data.frame(fpr=slot(performance(rocr.pred, "tpr", "fpr"),"x.values")[[1]], tpr=slot(performance(rocr.pred, "tpr", "fpr"),"y.values")[[1]])
ggplot(rocr.pred.df,aes(x=fpr)) +geom_line(aes(y=tpr),lwd=1)
}


# Improvements :
# Figure out what dependent variable is
# Should we use classification (two or multiclass?) or regression?
# Get half of this  code to run
# Use crossvalidation (lecture 7)


