##install.packages("klaR")
##install.packages("ICS")

library(MASS) ##for lda function
library(klaR) ##for partimat function to produce 2-D partition plots
library(ICS) ##for multivariate normality tests
library(palmerpenguins)

Data<-penguins
##exclude observations with missing info on gender, and columns 2 and 8 which are not body measurements
Data<-Data[complete.cases(Data[ , 7]),-c(2,8)]
##only include adelies. Remove column one since they are all same species
adelies<-Data[which(Data$species=="Adelie"),-1]

set.seed(49)

##create training and test data
sample.data<-sample.int(nrow(adelies), floor(.70*nrow(adelies)), replace = F)
train<-adelies[sample.data, ]
test<-adelies[-sample.data, ]

#################################################################
##Assumption is that the predictors follow a MVN for each class##
#################################################################

##subset dataframe by gender
gender1<-train[which(train$sex=="female"),]
gender2<-train[which(train$sex=="male"),]

##MVN tests for females
ICS::mvnorm.kur.test(gender1[,1:4])
ICS::mvnorm.skew.test(gender1[,1:4])

##MVN tests for males
ICS::mvnorm.kur.test(gender2[,1:4])
ICS::mvnorm.skew.test(gender2[,1:4])

#######
##EDA##
#######

##scatterplot matrix of predictors, different colors for each species
pairs(train[,1:4], col = c(1,2)[train$sex], lower.panel=NULL)


#######
##LDA##
#######

##check dummy coding for gender
contrasts(train$sex)

##Carry out LDA on training data
lda.adelies <- MASS::lda(sex ~ ., data=train)
##obtain ouput from LDA
lda.adelies

##See 1-D groupings by LD1
plot(lda.adelies, dimen = 1, type = "b")

##boundaries based on 2 predictors
klaR::partimat(sex ~ ., nplots.vert=2, nplots.hor=3, data=train, method="lda")

##predictions on training data. 
lda.train <- predict(lda.adelies)
##Confusion matrix on training data. Rows represent actual value, cols represent pred value
table(train$sex, lda.train$class)

##accuracy on training data
mean(train$sex == lda.train$class)

##predictions on test data. 
lda.test <- predict(lda.adelies,test)
##confusion matrix. By default, threshold is 0.5
table(test$sex,lda.test$class)

##accuracy on test data
mean(test$sex == lda.test$class)

##posterior probabilities for first 6 observations of test data
head(lda.test$posterior)

##threshold of 0.2
table(test$sex, lda.test$posterior[,2]>0.2)

##ROC and AUC
library(ROCR)
preds<-lda.test$posterior[,2]
rates<-ROCR::prediction(preds, test$sex)
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Gender of Adelie Penguins")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

#######
##QDA##
#######

##Use QDA on training data
qda.adelies <- MASS::qda(sex ~ ., train)
qda.adelies

##Quadratic boundaries based on 2 predictors
klaR::partimat(sex ~ ., data=train, method="qda")

##predictions on test data
qda.test <- predict(qda.adelies,test)
table(test$sex,qda.test$class)

##ROC and AUC
preds.qda<-qda.test$posterior[,2]
rates.qda<-ROCR::prediction(preds.qda, test$sex)
roc_result.qda<-ROCR::performance(rates.qda,measure="tpr", x.measure="fpr")
plot(roc_result.qda, main="ROC Curve for Gender of Adelie Penguins")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-ROCR::performance(rates.qda, measure = "auc")
auc@y.values
