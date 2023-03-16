##Fitting Classification Trees
library(randomForest) ##for random forests (and bagging)
library(gbm) ##for boosting

########################################################
##Convert response to become binary and data splitting##
########################################################

##Convert medv to a binary variable called High. The response must be a factor for the tree() function to know to fit a classification tree. If coded as 0/1, tree() will fit a regression tree.
library(MASS)
High<-ifelse(Boston$medv<median(Boston$medv), "No", "Yes")
High<-factor(High)
contrasts(High) ##yes is coded as 1

##add High to data frame, and remove the old response variable
Boston.new<-data.frame(Boston[,-14], High)

##Split data
set.seed(1)
sample.data<-sample.int(nrow(Boston.new), floor(.50*nrow(Boston.new)), replace = F)
train<-Boston.new[sample.data, ]
test<-Boston.new[-sample.data, ]

##store the response variable for test data. Use later to evaluate test MSE
pred.test<-test[,"High"]

##########
##Lab 8b##
##########

##############################
##Bagging and Random Forests##
##############################

set.seed(222)
##bagging is special case of random forest when mtry = number of predictors
bag.class<-randomForest::randomForest(High~., data=train, mtry=13, importance=TRUE)
bag.class ##note with classification tree OOB estimates are provided

##importance measures of predictors
randomForest::importance(bag.class)
##graphical version
randomForest::varImpPlot(bag.class)

##test accuracy with bagging
pred.bag<-predict(bag.class, newdata=test)
##confusion matrix for test data
table(pred.test, pred.bag)
mean(pred.bag==pred.test) ##0.889. Improvement over pruning.

##change threshold
##obtain predicted probabilities
pred.bag2<-predict(bag.class, newdata=test, type="prob")
##confusion matrix for threshold of 0.8
thresh8<-table(pred.test, pred.bag2[,2]>0.8)
thresh8

##Random Forest
set.seed(2222)
rf.class<-randomForest::randomForest(High~., data=train, mtry=3,importance=TRUE)
rf.class

importance(rf.class)
varImpPlot(rf.class)

##test accuracy with Random Forest
pred.rf<-predict(rf.class, newdata=test)
mean(pred.rf==pred.test) 

############
##Boosting##
############


##to use gbm() function, need to convert response to dummy codes
dummy<-ifelse(Boston.new$High=="No", 0, 1)

##add dummy coded response to data frame, and remove the response that is a factor
Boston.dummy<-data.frame(Boston.new[,-14], dummy)

##split data
train3<-Boston.dummy[sample.data, ]
test3<-Boston.dummy[-sample.data, ]

##store response
pred.test.dummy<-test3$dummy

set.seed(22222)
boost.class<-gbm::gbm(dummy~., data=train3, distribution="bernoulli", n.trees=500)
summary(boost.class)

plot(boost.class,i="rm")
plot(boost.class,i="lstat")

##this gives predicted probabilities, not predicted class. This is because the response is using 0/1 dummy codes with gbm()
pred.boost<-predict(boost.class, newdata=test3, n.trees=500, type = "response")

##confusion matrix
boost.tab<-table(pred.test.dummy, pred.boost>0.5)
boost.tab
##test accuracy with boosting
(boost.tab[1,1]+boost.tab[2,2])/sum(boost.tab) ##0.905.  

