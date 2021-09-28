library(ROCR)
Data<-read.table("students.txt", header=T)

##set seed so results are reproducible
set.seed(111)

########################################
##Split data to training and test data##
########################################

##evenly split data into train and test sets
sample.data<-sample.int(nrow(Data), floor(.50*nrow(Data)), replace = F)
train<-Data[sample.data, ]
test<-Data[-sample.data, ]

########################
##EDA on training data##
########################

##tabulate the number and proportion of students who have driven after drinking
table(train$DrivDrnk)
prop.table(table(train$DrivDrnk))

##consider gender and daysbeer as predictors for drvdrnk

##drvdrnk and daysbeer
boxplot(train$DaysBeer~train$DrivDrnk, xlab="Driven Drunk", ylab="Num of Days", main="Num of Days Drinking Beer by Driven Drunk")

##2 way table with gender & drivdrnk
mytab<-table(train$Gender, train$DrivDrnk)
mytab
prop.table(mytab, 1)

##check relationship between predictors
boxplot(train$DaysBeer~train$Gender, xlab="Gender", ylab="Num of Days", main="Num of Days Drinking Beer by Gender")

##################################
##check dummy coding of response##
##################################

contrasts(train$DrivDrnk)


###############################################
##fit logistic regression using training data##
###############################################

result_train<-glm(DrivDrnk~DaysBeer+Gender, family=binomial, data=train)

#######################################
##get output from logistic regression##
#######################################

summary(result_train)

#################################################################################
##Find delta Gsquared test statistic to compare model with intercept-only model##
#################################################################################

TS1<-result_train$null - result_train$dev

1-pchisq(TS1,2)

##4 predictor model

result_4<-glm(DrivDrnk~DaysBeer+Gender+Marijuan+StudyHrs, family=binomial, data=train)

TS2<-result_train$dev - result_4$dev

1-pchisq(TS2,2)

################################################
##Prediction for DaysBeer = 5 and male student##
################################################

newpred<-data.frame(DaysBeer=5, Gender="male", Marijuan="Yes", StudyHrs=10)
predict(result_4,newpred) ##estimated log odds
predict(result_4,newpred,type="response") ##estimated probability

#############
##ROC curve##
#############

##predicted survival rate for test data based on training data
preds<-predict(result_4,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$DrivDrnk)

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

###################
##compute the AUC##
###################

auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

##########################################
##confusion matrix when threshold is 0.5##
##########################################
  
confusion.mat<-table(test$DrivDrnk,preds > 0.5)
confusion.mat




